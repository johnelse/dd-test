let _dd = "/bin/dd"
let _sparse_dd = "/opt/xensource/libexec/sparse_dd"

let dd_program_of_string = function
	| "dd" -> _dd
	| "sparse_dd" -> _sparse_dd
	| _ -> failwith "Unknown dd program"

type mode_t =
	| Filesystem
	| Tapdevice

let mode_of_string = function
	| "filesystem" -> Filesystem
	| "tapdevice" -> Tapdevice
	| _ -> failwith "Unknown mode"

type config_t = {
	dd_program: string;
	mode: mode_t;
	source_vdi: API.ref_VDI;
	dest_sr: API.ref_SR;
}

open API
module XenAPI = Client.Client

let load ~rpc ~session_id =
	let dd_program = ref _sparse_dd in
	let mode = ref Filesystem in
	let source_vdi = ref (Ref.of_string "") in
	let dest_sr = ref (Ref.of_string "") in
	Arg.parse
		[
			("-dd-program",
				Arg.String (fun str -> dd_program := dd_program_of_string str),
				"The DD program to use");
			("-mode",
				Arg.String (fun str -> mode := mode_of_string str),
				"Determines whether to use filesystem or tap devices");
			("-src-vdi",
				Arg.String (fun str ->
					source_vdi := XenAPI.VDI.get_by_uuid ~rpc ~session_id ~uuid:str),
				"The VDI from which we will copy");
			("-dst-sr",
				Arg.String (fun str ->
					dest_sr := XenAPI.SR.get_by_uuid ~rpc ~session_id ~uuid:str),
				"The SR to which we will copy");
		]
		(fun _ -> ())
		"dd-test -dd-program [dd|sparse_dd] -mode [filesystem|tapdevice] -src-vdi ... -dst-sr ...";
	{
		dd_program = !dd_program;
		mode = !mode;
		source_vdi = !source_vdi;
		dest_sr = !dest_sr;
	}
