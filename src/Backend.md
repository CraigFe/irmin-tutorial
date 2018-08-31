# Writing a storage backend

This section illustrates how to write a custom storage backend for Irmin using a simplified implementation of [irmin-redis](https://github.com/zshipko/irmin-redis) as an example. `irmin-redis` uses a Redis server to store Irmin data.

Unlike writing a [custom datatype](Contents.html), there is not a tidy way of doing this. Each backend must fulfill certain criteria as defined by [Irmin.AO_MAKER](https://mirage.github.io/irmin/irmin/Irmin/module-type-AO_MAKER/index.html), [Irmin.LINK_MAKER](https://mirage.github.io/irmin/irmin/Irmin/module-type-LINK_MAKER/index.html), [Irmin.RW_MAKER](https://mirage.github.io/irmin/irmin/Irmin/module-type-RW_MAKER/index.html), [Irmin.S_MAKER](https://mirage.github.io/irmin/irmin/Irmin/module-type-S_MAKER/index.html), and [Irmin.KV_MAKER](https://mirage.github.io/irmin/irmin/Irmin/module-type-KV_MAKER/index.html). These module types define interfaces for functors that create stores. For example, a `KV_MAKER` defines a module that takes an `Irmin.Contents.S` as a parameter and returns a module of type `Irmin.KV`.

## Redis client

This examples uses the [hiredis](https://github.com/zshipko/ocaml-hiredis) package to create connections, send and receive data from Redis servers. It is available on [opam](https://github.com/ocaml/opam) under the same name.

## The readonly store

The process for writing a backend for Irmin requires implementing a few functors. First off, the ([RO](https://mirage.github.io/irmin/irmin/Irmin/module-type-RO/index.html)) store.

The [RO](https://mirage.github.io/irmin/irmin/Irmin/module-type-RO/index.html) module type requires the following types to be defined:

- `t`: The store type
- `key`: The key type
- `value`: The value/content type

```ocaml
open Lwt.Infix
open Hiredis
```

```ocaml
module RO (K: Irmin.Contents.Conv) (V: Irmin.Contents.Conv) = struct
  type t = (string * Client.t) (* Store type: Redis prefix and client *)
  type key = K.t               (* Key type *)
  type value = V.t             (* Value type *)
```

Additionally, it requires a few functions:

- `v`: used to create a value of type `t`
- `mem`: checks whether or not a key exists
- `find`: returns the value associated with a key (if it exists)

Since an Irmin database requires a few levels of store types (links, objects, etc...) a prefix is needed to identify the store type in Redis or else several functions will return incorrect results. This is not an issue with the in-memory backend, since it is easy to just create an independent store for each type, however in this case, there will be several diffent store types on a single Redis instance.


```ocaml
  let v prefix config =
    let module C = Irmin.Private.Conf in
    let root = match C.get config Irmin.Private.Conf.root with
      | Some root -> root ^ ":" ^ prefix ^ ":"
      | None -> prefix ^ ":"
    in
    Lwt.return (root, Client.connect ~port:6379 "127.0.0.1")
```

`mem` is implemented using the `EXISTS` command, which checks for the exitence of a key in Redis:

```ocaml
  let mem (prefix, client) key =
      let key = Fmt.to_to_string K.pp key in
      match Client.run client [| "EXISTS"; prefix ^ key |] with
      | Integer 1L -> Lwt.return_true
      | _ -> Lwt.return_false
```

`find` uses the `GET` command to retreive and key, if one isn't found or can't be decoded correctly then `find` returns `None`:

```ocaml
  let find (prefix, client) key =
      let key = Fmt.to_to_string K.pp key in
      match Client.run client [| "GET"; prefix ^ key |] with
      | String s ->
          (match V.of_string s with
          | Ok s -> Lwt.return_some s
          | _ -> Lwt.return_none)
      | _ -> Lwt.return_none
end
```

### The append-only store

Next is the append-only ([AO](https://mirage.github.io/irmin/irmin/Irmin/module-type-AO/index.html)) interface - the majority of the required methods can be inherited from `RO`!

```ocaml
module AO (K: Irmin.Hash.S) (V: Irmin.Contents.Conv) = struct
  include RO(K)(V)
  let v = v "obj"
```

This module needs an `add` function, which takes a value, hashes it, stores the association and returns the hash:

```ocaml
  let add (prefix, client) value =
      let hash = K.digest V.t value in
      let key = Fmt.to_to_string K.pp hash in
      let value = Fmt.to_to_string V.pp value in
      ignore (Client.run client [| "SET"; prefix ^ key; value |]);
      Lwt.return hash
end
```

### The link store

The [Link](https://mirage.github.io/irmin/irmin/Irmin/module-type-LINK/index.html) store creates verified links between low-level keys. The link store doesn't know about the type of value you're storing, it is only interesting in creating linking keys together.

```ocaml
module Link (K: Irmin.Hash.S) = struct
  include RO(K)(K)
  let v = v "link"
```

This `add` function is different from the append-only store implementation. It takes two key arguments (`index` and `key`) and stores the association from `index` to `key`:

```ocaml
  let add (prefix, client) index key =
      let key = Fmt.to_to_string K.pp key in
      let index = Fmt.to_to_string K.pp index in
      ignore (Client.run client [| "SET"; prefix ^ index; key |]);
      Lwt.return_unit
end
```

## The read-write store

The [RW](https://mirage.github.io/irmin/irmin/Irmin/module-type-RW/index.html) store has many more types and values that need to be defined than the previous examples, but luckily this is the last step!

To start off we can use the `RO` functor defined above to create a `RO` module:

```ocaml
module RW (K: Irmin.Contents.Conv) (V: Irmin.Contents.Conv) = struct
  module RO = RO(K)(V)
```

There are a few types we need to declare next. `key` and `value` should match `RO.key` and `RO.value` and `watch` is used to declare the type of the watcher -- this is used to send notifications when the store has been updated. [irmin-watcher](https://github.com/mirage/irmin-watcher) has some more information on watchers.

```ocaml
  module W = Irmin.Private.Watch.Make(K)(V)
  type t = { t: RO.t; w: W.t }  (* Store type *)
  type key = RO.key             (* Key type *)
  type value = RO.value         (* Value type *)
  type watch = W.watch          (* Watch type *)
```

The `watches` variable defined below creates a context used to track active watches.

```ocaml
  let watches = W.v ()
```

Again, we need a `v` function for creating a value of type `t`:

```ocaml
  let v config =
    RO.v "data" config >>= fun t ->
    Lwt.return {t; w = watches }
```

The next few functions (`find` and `mem`) are just wrappers around the implementations in `RO`:

```ocaml
  let find t = RO.find t.t
  let mem t  = RO.mem t.t
```

A few more simple functions: `watch_key`, `watch` and `unwatch`, used to created or destroy watches:

```ocaml
  let watch_key t key = W.watch_key t.w key
  let watch t = W.watch t.w
  let unwatch t = W.unwatch t.w
```

We will need to implement a few more functions:

- `list`, lists files at a specific path.
- `set`, writes a value to the store.
- `remove`, deletes a value from the store.
- `test_and_set`, modifies a key only if the `test` value matches the current value for the given key.

The `list` implementation will get a list of keys from Redis using the `KEYS` command then convert them from strings to `Store.key` values:

```ocaml
  let list {t = (prefix, client); _} =
      match Client.run client [| "KEYS"; prefix ^ "*" |] with
      | Array arr ->
          Array.map (fun k ->
            K.of_string (Value.to_string k)
          ) arr
          |> Array.to_list
          |> Lwt_list.filter_map_s (function
            | Ok s -> Lwt.return_some s
            | _ -> Lwt.return_none)
      | _ -> Lwt.return []
```

`set` just encodes the keys and values as strings, then uses the Redis `SET` command to store them:

```ocaml
  let set {t = (prefix, client); w} key value =
      let key' = Fmt.to_to_string K.pp key in
      let value' = Fmt.to_to_string V.pp value in
      match Client.run client [| "SET"; prefix ^ key'; value' |] with
      | Status "OK" -> W.notify w key (Some value)
      | _ -> Lwt.return_unit
```

`remove` uses the Redis `DEL` command to remove stored values:

```ocaml
  let remove {t = (prefix, client); w} key =
      let key' = Fmt.to_to_string K.pp key in
      ignore (Client.run client [| "DEL"; prefix ^ key' |]);
      W.notify w key None
```

`test_and_set` will modify a key if the current value is equal to `test`. This requires an atomic check and set, which can be done using `WATCH`, `MULTI` and `EXEC` in Redis:

```ocaml
  let test_and_set t key ~test ~set:set_value =
    (* A helper function to execute a command in a Redis transaction *)
    let txn client args =
      ignore @@ Client.run client [| "MULTI" |];
      ignore @@ Client.run client args;
      Client.run client [| "EXEC" |] <> Nil
    in
    let prefix, client = t.t in
    let key' = Fmt.to_to_string K.pp key in
    (* Start watching the key in question *)
    ignore @@ Client.run client [| "WATCH"; prefix ^ key' |];
    (* Get the existing value *)
    find t key >>= fun v ->
    (* Check it against [test] *)
    if Irmin.Type.(equal (option V.t)) test v then (
      (match set_value with
        | None -> (* Remove the key *)
            if txn client [| "DEL"; prefix ^ key' |] then
              W.notify t.w key None >>= fun () ->
              Lwt.return_true
            else
              Lwt.return_false
        | Some value -> (* Update the key *)
            let value' = Fmt.to_to_string V.pp value in
            if txn client [| "SET"; prefix ^ key'; value' |] then
              W.notify t.w key set_value >>= fun () ->
              Lwt.return_true
            else
              Lwt.return_false
      ) >>= fun ok ->
      Lwt.return ok
    ) else (
      ignore @@ Client.run client [| "UNWATCH"; prefix ^ key' |];
      Lwt.return_false
    )
end
```

Finally, add `Make` and `KV` functors for creating Redis-backed Irmin stores:

```ocaml
module Make = Irmin.Make(AO)(RW)

module KV (C: Irmin.Contents.S) : Irmin.KV_MAKER =
  Make
    (Irmin.Metadata.None)
    (C)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
```

