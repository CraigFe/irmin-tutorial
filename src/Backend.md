# Writing a storage backend

This section illustrates how to write a custom storage backend for Irmin using a simplified implementation of [irmin-redis](https://github.com/zshipko/irmin-redis) as an example. `irmin-redis` uses a Redis server to store Irmin data.

Unlike writing a [custom datatype](Contents.html), there is not a tidy way of doing this. Each backend must fulfill certain criteria as defined by [Irmin.CONTENT_ADDRESSABLE_STORE_MAKER](https://mirage.github.io/irmin/irmin/Irmin/module-type-CONTENT_ADDRESSABLE_STORE_MAKER/index.html), [Irmin.ATOMIC_WRITE_STORE_MAKER](https://mirage.github.io/irmin/irmin/Irmin/module-type-ATOMIC_WRITE_STORE_MAKER/index.html), [Irmin.S_MAKER](https://mirage.github.io/irmin/irmin/Irmin/module-type-S_MAKER/index.html), and [Irmin.KV_MAKER](https://mirage.github.io/irmin/irmin/Irmin/module-type-KV_MAKER/index.html). These module types define interfaces for functors that create stores. For example, a `KV_MAKER` defines a module that takes an `Irmin.Contents.S` as a parameter and returns a module of type `Irmin.KV`.

## Redis client

This example uses the [hiredis](https://github.com/zshipko/ocaml-hiredis) package to create connections, send and receive data from Redis servers. It is available on [opam](https://github.com/ocaml/opam) under the same name.

## The readonly store

The process for writing a backend for Irmin requires implementing a few functors -- the accomplish this, we can start off by writing a helper module that provides a generic implementation that can be re-used by the content-addressable store and the atomic-write store:


- `t`: the store type
- `key`: the key type
- `value`: the value/content type

```ocaml
open Lwt.Infix
open Hiredis
```

```ocaml
module Helper (K: Irmin.Type.S) (V: Irmin.Type.S) = struct
  type 'a t = (string * Client.t) (* Store type: Redis prefix and client *)
  type key = K.t                  (* Key type *)
  type value = V.t                (* Value type *)
```

Additionally, it requires a few functions:

- `v`: used to create a value of type `t`
- `mem`: checks whether or not a key exists
- `find`: returns the value associated with a key (if it exists)

Since an Irmin database requires a few levels of store types (links, objects, etc...) a prefix is needed to identify the store type in Redis or else several functions will return incorrect results. This is not an issue with the in-memory backend, since it is easy to just create an independent store for each type, however in this case, there will be several different store types on a single Redis instance.


```ocaml
  let v prefix config =
    let module C = Irmin.Private.Conf in
    let root = match C.get config C.root with
      | Some root -> root ^ ":" ^ prefix ^ ":"
      | None -> prefix ^ ":"
    in
    Lwt.return (root, Client.connect ~port:6379 "127.0.0.1")
```

`mem` is implemented using the `EXISTS` command, which checks for the existence of a key in Redis:

```ocaml
  let mem (prefix, client) key =
      let key = Irmin.Type.to_string K.t key in
      match Client.run client [| "EXISTS"; prefix ^ key |] with
      | Integer 1L -> Lwt.return_true
      | _ -> Lwt.return_false
```

`find` uses the `GET` command to retrieve the key, if one isn't found or can't be decoded correctly then `find` returns `None`:

```ocaml
  let find (prefix, client) key =
      let key = Irmin.Type.to_string K.t key in
      match Client.run client [| "GET"; prefix ^ key |] with
      | String s ->
          (match Irmin.Type.of_string V.t s with
          | Ok s -> Lwt.return_some s
          | _ -> Lwt.return_none)
      | _ -> Lwt.return_none
end
```

### The content-addressable store

Next is the content-addressable ([CONTENT_ADDRESSABLE_STORE](https://mirage.github.io/irmin/irmin/Irmin/module-type-CONTENT_ADDRESSABLE_STORE/index.html)) interface - the majority of the required methods can be inherited from `Helper`!

```ocaml
module Content_addressable (K: Irmin.Hash.S) (V: Irmin.Type.S) = struct
  include Helper(K)(V)
  let v = v "obj"
```

This module needs an `add` function, which takes a value, hashes it, stores the association and returns the hash:

```ocaml
  let add (prefix, client) value =
      let hash = K.hash (fun f -> f @@ Irmin.Type.to_string V.t value) in
      let key = Irmin.Type.to_string K.t hash in
      let value = Irmin.Type.to_string V.t value in
      ignore (Client.run client [| "SET"; prefix ^ key; value |]);
      Lwt.return hash
```

Then a `batch` function, which can be used to group writes together. We will use the most basic implementation:

```ocaml
  let batch (prefix, client) f =
    let _ = Client.run client [| "MULTI" |] in
    f (prefix, client) >|= fun result ->
    let _ = Client.run client [| "EXEC" |] in
    result
end
```

## The atomic-write store

The [ATOMIC_WRITE_STORE](https://mirage.github.io/irmin/irmin/Irmin/module-type-ATOMIC_WRITE_STORE/index.html) has many more types and values that need to be defined than the previous examples, but luckily this is the last step!

To start off we can use the `Helper` functor defined above:

```ocaml
module Atomic_write (K: Irmin.Type.S) (V: Irmin.Type.S) = struct
  module H = Helper(K)(V)
```

There are a few types we need to declare next. `key` and `value` should match `H.key` and `H.value` and `watch` is used to declare the type of the watcher -- this is used to send notifications when the store has been updated. [irmin-watcher](https://github.com/mirage/irmin-watcher) has some more information on watchers.

```ocaml
  module W = Irmin.Private.Watch.Make(K)(V)
  type t = { t: [`Write] H.t; w: W.t }  (* Store type *)
  type key = H.key                      (* Key type *)
  type value = H.value                  (* Value type *)
  type watch = W.watch                  (* Watch type *)
```

The `watches` variable defined below creates a context used to track active watches.

```ocaml
  let watches = W.v ()
```

Again, we need a `v` function for creating a value of type `t`:

```ocaml
  let v config =
    H.v "data" config >>= fun t ->
    Lwt.return {t; w = watches }
```

The next few functions (`find` and `mem`) are just wrappers around the implementations in `H`:

```ocaml
  let find t = H.find t.t
  let mem t  = H.mem t.t
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
            Irmin.Type.of_string K.t (Value.to_string k)
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
      let key' = Irmin.Type.to_string K.t key in
      let value' = Irmin.Type.to_string V.t value in
      match Client.run client [| "SET"; prefix ^ key'; value' |] with
      | Status "OK" -> W.notify w key (Some value)
      | _ -> Lwt.return_unit
```

`remove` uses the Redis `DEL` command to remove stored values:

```ocaml
  let remove {t = (prefix, client); w} key =
      let key' = Irmin.Type.to_string K.t key in
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
    let key' = Irmin.Type.to_string K.t key in
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
            let value' = Irmin.Type.to_string V.t value in
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
module Make: Irmin.S_MAKER = Irmin.Make(Content_addressable)(Atomic_write)

module KV: Irmin.KV_MAKER = functor (C: Irmin.Contents.S) ->
  Make
    (Irmin.Metadata.None)
    (C)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
```

