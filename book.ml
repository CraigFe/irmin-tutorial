#31 "src/Contents.md"
module Counter: Irmin.Contents.S with type t = int64 = struct
	type t = int64
	let t = Irmin.Type.int64
;;
#39 "src/Contents.md"
	let pp fmt = Format.fprintf fmt "%Ld"
;;
#45 "src/Contents.md"
	let of_string s =
		match Int64.of_string_opt s with
		| Some i -> Ok i
		| None -> Error (`Msg "invalid counter value")
;;
#56 "src/Contents.md"
	let merge ~old a b =
	    let open Irmin.Merge.Infix in
		old () >|=* fun old ->
        let old = match old with None -> 0L | Some o -> o in
        let (+) = Int64.add and (-) = Int64.sub in
        a + b - old
;;
#65 "src/Contents.md"
    let merge = Irmin.Merge.(option (v t merge))
end
;;
#72 "src/Contents.md"
let merge = Irmin.Merge.(option counter)
;;
#78 "src/Contents.md"
module Counter_mem_store = Irmin_mem.KV(Counter)
;;
#88 "src/Contents.md"
type color =
    | Black
    | White
    | Other of string
type car = {
    license: string;
    year: int32;
    make_and_model: string * string;
    color: color;
    owner: string;
}
;;
#104 "src/Contents.md"
module Car = struct
    type t  = car
    let color =
        let open Irmin.Type in
        variant "color" (fun black white other -> function
            | Black -> black
            | White -> white
            | Other color -> other color)
        |~ case0 "Black" Black
        |~ case0 "White" White
        |~ case1 "Other" string (fun s -> Other s)
        |> sealv
;;
#121 "src/Contents.md"
    let t =
        let open Irmin.Type in
        record "car" (fun license year make_and_model color owner ->
            {license; year; make_and_model; color; owner})
        |+ field "license" string (fun t -> t.license)
        |+ field "year" int32 (fun t -> t.year)
        |+ field "make_and_model" (pair string string) (fun t -> t.make_and_model)
        |+ field "color" color (fun t -> t.color)
        |+ field "owner" string (fun t -> t.owner)
        |> sealr
;;
#136 "src/Contents.md"
	let pp = Irmin.Type.pp_json t
;;
#142 "src/Contents.md"
    let of_string s =
        let decoder = Jsonm.decoder (`String s) in
        Irmin.Type.decode_json t decoder
;;
#150 "src/Contents.md"
    let merge = Irmin.Merge.(option (idempotent t))
end
;;
#157 "src/Contents.md"
open Lwt.Infix
module Car_store = Irmin_mem.KV(Car)

let car_a = {
    color = Other "green";
    license = "ABCD123";
    year = 2002l;
    make_and_model = ("Honda", "Accord");
    owner = "Jane Doe";
}

let car_b = {
    color = Black;
    license = "MYCAR00";
    year = 2016l;
    make_and_model = ("Toyota", "Corolla");
    owner = "Mike Jones";
}

let add_car store car_number car =
    let info = Irmin_unix.info "added %s" car_number in
    Car_store.set_exn store [car_number] car ~info

let main =
    let config = Irmin_mem.config () in
    Car_store.Repo.v config >>= Car_store.master >>= fun t ->
    add_car t "5Y2SR67049Z456146" car_a >>= fun () ->
    add_car t "2FAFP71W65X110910" car_b >>= fun () ->
    Car_store.get t ["2FAFP71W65X110910"] >|= fun car ->
    assert (car.license = car_b.license);
    assert (car.year = car_b.year)

let () = Lwt_main.run main
;;
#199 "src/Contents.md"
module Object = struct
    type t = (string * string) list
    let t = Irmin.Type.(list (pair string string))
;;
#209 "src/Contents.md"
	let pp = Irmin.Type.pp_json t
;;
#215 "src/Contents.md"
    let of_string s =
        let decoder = Jsonm.decoder (`String s) in
        Irmin.Type.decode_json t decoder
;;
#224 "src/Contents.md"
    let merge_alist =
        Irmin.Merge.(alist Irmin.Type.string Irmin.Type.string (fun _key -> option string))
    let merge = Irmin.Merge.(option merge_alist)
end
;;
#237 "src/Contents.md"
module type TIMESTAMP = sig
    val now: unit -> int64
end
;;
#244 "src/Contents.md"
module Timestamp = struct
    let now () = Int64.of_float @@ Unix.gettimeofday () *. 100000.
end
;;
#252 "src/Contents.md"
module Lww_register (Time: TIMESTAMP) (C: Irmin.Type.S) = struct
    type t = C.t * int64
    let t =
        Irmin.Type.(pair C.t int64)
;;
#260 "src/Contents.md"
    let v c = (c, Time.now ())
;;
#266 "src/Contents.md"
	let pp = Irmin.Type.pp_json t
    let of_string s =
        let decoder = Jsonm.decoder (`String s) in
        Irmin.Type.decode_json t decoder
;;
#275 "src/Contents.md"
    let merge ~old (a, timestamp_a) (b, timestamp_b) =
        match Int64.compare timestamp_a timestamp_b with
        | 0 ->
            if Irmin.Type.equal C.t a b then
                Irmin.Merge.ok (a, timestamp_a)
            else
                let msg = "Conflicting entries have the same timestamp but different values" in
                Irmin.Merge.conflict "%s" msg
        | 1 -> Irmin.Merge.ok (a, timestamp_a)
        | _ -> Irmin.Merge.ok (b, timestamp_b)
    let merge = Irmin.Merge.(option (v t merge))
end
;;
#292 "src/Contents.md"
open Lwt.Infix
module Value = Lww_register(Timestamp)(Irmin.Contents.String)
module Store = Irmin_mem.KV(Value)
let main =
    (* Configure the repo *)
    let cfg = Irmin_mem.config () in
    (* Access the master branch *)
    Store.Repo.v cfg >>= Store.master >>= fun master ->
    (* Set [foo] to ["bar"] on master branch *)
    Store.set_exn master ["foo"] (Value.v "bar") ~info:(Irmin_unix.info "set foo on master branch") >>= fun () ->
    (* Access example branch *)
    Store.Repo.v cfg >>= fun repo -> Store.of_branch repo "example" >>= fun example ->
    (* Set [foo] to ["baz"] on example branch *)
    Store.set_exn example ["foo"] (Value.v "baz") ~info:(Irmin_unix.info "set foo on example branch") >>= fun () ->
    (* Merge the example into master branch *)
    Store.merge_into ~into:master example ~info:(Irmin_unix.info "merge example into master") >>= function
    | Ok () ->
        (* Check that [foo] is set to ["baz"] after the merge *)
        Store.get master ["foo"] >|= fun (foo, _) ->
        assert (foo = "baz")
    | Error conflict ->
        let fmt = Irmin.Type.pp_json Irmin.Merge.conflict_t in
        Lwt_io.printl (Fmt.to_to_string fmt conflict)
let () = Lwt_main.run main
;;
#16 "src/GettingStartedOCaml.md"
module Mem_store = Irmin_mem.KV(Irmin.Contents.String)
;;
#22 "src/GettingStartedOCaml.md"
module Git_store = Irmin_unix.Git.FS.KV(Irmin.Contents.Json)
;;
#30 "src/GettingStartedOCaml.md"
module Mem_Store =
    Irmin_mem.Make
        (Irmin.Metadata.None)
        (Irmin.Contents.Json)
        (Irmin.Path.String_list)
        (Irmin.Branch.String)
        (Irmin.Hash.SHA1)
;;
#44 "src/GettingStartedOCaml.md"
let git_config = Irmin_git.config ~bare:true "/tmp/irmin"
;;
#48 "src/GettingStartedOCaml.md"
let config = Irmin_mem.config ()
;;
#54 "src/GettingStartedOCaml.md"
let git_repo = Git_store.Repo.v git_config
;;
#58 "src/GettingStartedOCaml.md"
let repo = Mem_store.Repo.v config
;;
#68 "src/GettingStartedOCaml.md"
open Lwt.Infix

let master config =
    Mem_store.Repo.v config >>= Mem_store.master
;;
#77 "src/GettingStartedOCaml.md"
let branch config name =
    Mem_store.Repo.v config >>= fun repo ->
    Mem_store.of_branch repo name
;;
#87 "src/GettingStartedOCaml.md"
let info message = Irmin_unix.info ~author:"Example" "%s" message

let main =
    Mem_store.Repo.v config >>= Mem_store.master >>= fun t ->
    (* Set a/b/c to "Hello, Irmin!" *)
    Mem_store.set_exn t ["a"; "b"; "c"] "Hello, Irmin!" ~info:(info "my first commit") >>= fun () ->
    (* Get a/b/c *)
    Mem_store.get t ["a"; "b"; "c"] >|= fun s ->
    assert (s = "Hello, Irmin!")
let () = Lwt_main.run main
;;
#104 "src/GettingStartedOCaml.md"
let transaction_example =
Mem_store.Repo.v config >>= Mem_store.master >>= fun t ->
let info = Irmin_unix.info "example transaction" in
Mem_store.with_tree_exn t [] ~info ~strategy:`Set (fun tree ->
    let tree = match tree with Some t -> t | None -> Mem_store.Tree.empty in
    Mem_store.Tree.remove tree ["foo"; "bar"] >>= fun tree ->
    Mem_store.Tree.add tree ["a"; "b"; "c"] "123" >>= fun tree ->
    Mem_store.Tree.add tree ["d"; "e"; "f"] "456" >>= Lwt.return_some)
let () = Lwt_main.run transaction_example
;;
#120 "src/GettingStartedOCaml.md"
let move t ~src ~dest =
    Mem_store.with_tree_exn t Mem_store.Key.empty ~strategy:`Set (fun tree ->
        match tree with
        | Some tr ->
            Mem_store.Tree.get_tree tr src >>= fun v ->
            Mem_store.Tree.remove tr src >>= fun _ ->
            Mem_store.Tree.add_tree tr dest v >>= Lwt.return_some
        | None -> Lwt.return_none
    )
let main =
    Mem_store.Repo.v config >>= Mem_store.master >>= fun t ->
    let info = Irmin_unix.info "move a -> foo" in
    move t ~src:["a"] ~dest:["foo"] ~info
let () = Lwt_main.run main
;;
#149 "src/GettingStartedOCaml.md"
open Irmin_unix
module Git_mem_store = Git.Mem.KV(Irmin.Contents.String)
module Sync = Irmin.Sync(Git_mem_store)
let remote = Git_mem_store.remote "git://github.com/mirage/irmin.git"
let main =
    Git_mem_store.Repo.v config >>= Git_mem_store.master >>= fun t ->
    Sync.pull_exn t remote `Set >>= fun () ->
    Git_mem_store.list t [] >|= List.iter (fun (step, kind) ->
        match kind with
        | `Contents -> Printf.printf "FILE %s\n" step
        | `Node -> Printf.printf "DIR %s\n" step
    )
let () = Lwt_main.run main
;;
#173 "src/GettingStartedOCaml.md"
module Mem_store_json = Irmin_mem.KV(Irmin.Contents.Json)
module Mem_store_json_value = Irmin_mem.KV(Irmin.Contents.Json_value)
;;
#180 "src/GettingStartedOCaml.md"
let main =
    let module Store = Mem_store_json_value in
    Store.Repo.v config >>= Store.master >>= fun t ->
    let value = `O ["x", `Float 1.; "y", `Float 2.; "z", `Float 3.] in
    Store.set_exn t ["a"; "b"; "c"] value ~info:(info "set a/b/c") >>= fun () ->
    Store.get t ["a"; "b"; "c"] >|= fun x ->
    assert (Irmin.Type.equal Store.contents_t value x)
let () = Lwt_main.run main
;;
#195 "src/GettingStartedOCaml.md"
let main =
    let module Store = Mem_store_json_value in
    let module Proj = Irmin.Json_tree(Store) in
    Store.Repo.v config >>= Store.master >>= fun t ->
    let value = `O ["test", `O ["foo", `String "bar"]; "x", `Float 1.; "y", `Float 2.; "z", `Float 3.] in
    Proj.set t ["a"; "b"; "c"] value ~info:(info "set a/b/c") >>= fun () ->
    Store.get t ["a"; "b"; "c"; "x"] >>= fun x ->
    assert (Irmin.Type.equal Store.contents_t (`Float 1.) x);
    Store.get t ["a"; "b"; "c"; "test"; "foo"] >>= fun x ->
    assert (Irmin.Type.equal Store.contents_t (`String "bar") x);
    Proj.get t ["a"; "b"] >|= fun x ->
    assert (Irmin.Type.equal Store.contents_t (`O ["c", value]) x)
let () = Lwt_main.run main
;;
#21 "src/Backend.md"
open Lwt.Infix
open Hiredis
;;
#26 "src/Backend.md"
module Helper (K: Irmin.Type.S) (V: Irmin.Type.S) = struct
  type 'a t = (string * Client.t) (* Store type: Redis prefix and client *)
  type key = K.t               (* Key type *)
  type value = V.t             (* Value type *)
;;
#42 "src/Backend.md"
  let v prefix config =
    let module C = Irmin.Private.Conf in
    let root = match C.get config C.root with
      | Some root -> root ^ ":" ^ prefix ^ ":"
      | None -> prefix ^ ":"
    in
    Lwt.return (root, Client.connect ~port:6379 "127.0.0.1")
;;
#54 "src/Backend.md"
  let mem (prefix, client) key =
      let key = Irmin.Type.to_string K.t key in
      match Client.run client [| "EXISTS"; prefix ^ key |] with
      | Integer 1L -> Lwt.return_true
      | _ -> Lwt.return_false
;;
#64 "src/Backend.md"
  let find (prefix, client) key =
      let key = Irmin.Type.to_string K.t key in
      match Client.run client [| "GET"; prefix ^ key |] with
      | String s ->
          (match Irmin.Type.of_string V.t s with
          | Ok s -> Lwt.return_some s
          | _ -> Lwt.return_none)
      | _ -> Lwt.return_none
end
;;
#80 "src/Backend.md"
module Content_adressable (K: Irmin.Hash.S) (V: Irmin.Type.S) = struct
  include Helper(K)(V)
  let v = v "obj"
;;
#88 "src/Backend.md"
  let add (prefix, client) value =
      let hash = K.digest (Irmin.Type.to_string V.t value) in
      let key = Irmin.Type.to_string K.t hash in
      let value = Irmin.Type.to_string V.t value in
      ignore (Client.run client [| "SET"; prefix ^ key; value |]);
      Lwt.return hash
;;
#99 "src/Backend.md"
  let batch (prefix, client) f =
    let _ = Client.run client [| "MULTI" |] in
    f (prefix, client) >|= fun result ->
    let _ = Client.run client [| "EXEC" |] in
    result
end
;;
#114 "src/Backend.md"
module Atomic_write (K: Irmin.Type.S) (V: Irmin.Type.S) = struct
  module H = Helper(K)(V)
;;
#121 "src/Backend.md"
  module W = Irmin.Private.Watch.Make(K)(V)
  type t = { t: [`Write] H.t; w: W.t }  (* Store type *)
  type key = H.key             (* Key type *)
  type value = H.value         (* Value type *)
  type watch = W.watch          (* Watch type *)
;;
#131 "src/Backend.md"
  let watches = W.v ()
;;
#137 "src/Backend.md"
  let v config =
    H.v "data" config >>= fun t ->
    Lwt.return {t; w = watches }
;;
#145 "src/Backend.md"
  let find t = H.find t.t
  let mem t  = H.mem t.t
;;
#152 "src/Backend.md"
  let watch_key t key = W.watch_key t.w key
  let watch t = W.watch t.w
  let unwatch t = W.unwatch t.w
;;
#167 "src/Backend.md"
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
;;
#183 "src/Backend.md"
  let set {t = (prefix, client); w} key value =
      let key' = Irmin.Type.to_string K.t key in
      let value' = Irmin.Type.to_string V.t value in
      match Client.run client [| "SET"; prefix ^ key'; value' |] with
      | Status "OK" -> W.notify w key (Some value)
      | _ -> Lwt.return_unit
;;
#194 "src/Backend.md"
  let remove {t = (prefix, client); w} key =
      let key' = Irmin.Type.to_string K.t key in
      ignore (Client.run client [| "DEL"; prefix ^ key' |]);
      W.notify w key None
;;
#203 "src/Backend.md"
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
;;
#244 "src/Backend.md"
module Make: Irmin.S_MAKER = Irmin.Make(Content_adressable)(Atomic_write)

module KV: Irmin.KV_MAKER = functor (C: Irmin.Contents.S) ->
  Make
    (Irmin.Metadata.None)
    (C)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
;;
#161 "src/GraphQL.md"
module Graphql_store = Irmin_unix.Git.Mem.KV(Irmin.Contents.String)
module Graphql = Irmin_unix.Graphql.Server.Make(Graphql_store)(struct let remote = Some Graphql_store.remote end)
;;
#168 "src/GraphQL.md"
let run_server () =
  (* Set up the Irmin store *)
  Graphql_store.Repo.v (Irmin_git.config "/tmp/irmin") >>= fun repo ->

  (* Initialize the GraphQL server *)
  let server = Graphql.v repo in

  (* Run the server *)
  let on_exn exn =
    Logs.debug (fun l -> l "on_exn: %s" (Printexc.to_string exn))
  in
  Cohttp_lwt_unix.Server.create ~on_exn ~mode:(`TCP (`Port 1234)) server
;;
#188 "src/GraphQL.md"
module Example_type = struct
  type t = {x: string; y: string; z: string}
  let t =
    let open Irmin.Type in
    record "Example_type" (fun x y z -> {x; y; z})
    |+ field "x" string (fun t -> t.x)
    |+ field "y" string (fun t -> t.y)
    |+ field "z" string (fun t -> t.z)
    |> sealr
  let merge = Irmin.Merge.(option (default t))
end

module Example_presenter = struct
  type t = Example_type.t
  type src = Example_type.t
  let to_src t = t
  let schema_typ =
    let open Example_type in
    Irmin_graphql.Server.Schema.(obj "Example"
      ~fields:(fun _ -> [
        field "x"
          ~typ:(non_null string)
          ~args:[]
          ~resolve:(fun _ t -> t.x)
        ;
        field "y"
          ~typ:(non_null string)
          ~args:[]
          ~resolve:(fun _ t -> t.y)
        ;
        field "z"
          ~typ:(non_null string)
          ~args:[]
          ~resolve:(fun _ t -> t.z)
        ;
      ])
    )
end
;;
#233 "src/GraphQL.md"
module Example_store = Irmin_mem.KV(Example_type)

module Presentation = struct
  module Contents = Example_presenter
  module Metadata = Irmin_graphql.Server.Default_presenter(Example_store.Metadata)
end

module Config = struct
  let remote = None
  let info = Irmin_unix.info
end

module Graphql_ext =
  Irmin_graphql.Server.Make_ext
    (Cohttp_lwt_unix.Server)
    (Config)
    (Example_store)
    (Presentation)
;;
