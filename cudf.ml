(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009  Stefano Zacchiroli <zack@pps.jussieu.fr>             *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU General Public License as published by     *)
(*  the Free Software Foundation, either version 3 of the License, or (at    *)
(*  your option) any later version.                                          *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful, but      *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of               *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        *)
(*  General Public License for more details.                                 *)
(*                                                                           *)
(*  You should have received a copy of the GNU General Public License        *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*****************************************************************************)

type version = int
type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]

type pkgname = string
type vpkg = pkgname * ([`Eq|`Neq|`Geq|`Gt|`Leq|`Lt] * version) option
type vpkglist = vpkg list
type vpkgformula =
    FTrue
  | FPkg of vpkg
  | FOr of vpkgformula list
  | FAnd of vpkgformula list
type veqpkg = pkgname * ([`Eq] * version) option
type veqpkglist = veqpkg list
type enum_keep = [ `Keep_version | `Keep_package | `Keep_feature ]

type package = {
  package : pkgname ;
  version : version ;
  depends : vpkgformula ;
  conflicts : vpkglist ;
  provides : veqpkglist ;
  installed : bool ;
  keep : [ `Keep_version | `Keep_package | `Keep_feature ] option ;
  extra : (string * string) list
}
type request = {
  problem_id : string ;
  install : vpkglist ;
  remove : vpkglist ;
  upgrade : vpkglist ;
}
type cudf = package list * request

let lookup_package (pkgs, _req) id =
  List.find (fun pkg -> (pkg.package, pkg.version) = id) pkgs
