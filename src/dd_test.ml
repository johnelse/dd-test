let xapi_rpc request =
	Rpc_client.do_rpc_unix
		~content_type:(Rpc_client.content_type_of_string "text/xml")
		~filename:(Filename.concat "/var/lib/xcp" "xapi")
		~path:"/" request

open API
module XenAPI = Client.Client

exception Sr_unsupported

let get_vhd_path ~rpc ~session_id ~sr ~vdi =
	let sr_type = XenAPI.SR.get_type ~rpc ~session_id ~self:sr in
	match sr_type with
	| "nfs" | "ext" ->
		Printf.sprintf "/var/run/sr-mount/%s/%s"
			(XenAPI.SR.get_uuid ~rpc ~session_id ~self:sr)
			(XenAPI.VDI.get_uuid ~rpc ~session_id ~self:vdi)
	| "lvm" | "lvmohba" | "lvmoiscsi" ->
		Printf.sprintf "/dev/sm/backend/%s/%s"
			(XenAPI.SR.get_uuid ~rpc ~session_id ~self:sr)
			(XenAPI.VDI.get_uuid ~rpc ~session_id ~self:vdi)
	| _ ->
		raise Sr_unsupported

let rec find_existing_file ~files =
	match files with
	| [] -> raise Not_found
	| f :: rest ->
		if Sys.file_exists f
		then f
		else find_existing_file ~files:rest

let possible_inventory_files = [
	"/etc/xensource-inventory";
	"/etc/xcp/inventory";
]

let get_dom0 ~rpc ~session_id =
	Inventory.inventory_filename :=
		find_existing_file ~files:possible_inventory_files;
	let control_domain_uuid = Inventory.lookup Inventory._control_domain_uuid in
	XenAPI.VM.get_by_uuid ~rpc ~session_id ~uuid:control_domain_uuid

let () =
	let rpc = xapi_rpc in
	let session_id = XenAPI.Session.login_with_password
		~rpc ~uname:"dd-test" ~pwd:"" ~version:"1.1" in
	let _ = Config.load ~rpc ~session_id in
	let dom0 = get_dom0 ~rpc ~session_id in
	print_endline (Ref.of_string dom0)
