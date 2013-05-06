module XenAPI = Client.Client

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
