let year = 2023
let day = 1

module Part_1 = struct
  let fst_digit ?(rev = false) s =
    let rec h (f : int -> int) (stop_n : int) (i : int) =
      if stop_n = i then 0
      else if Char.code s.[i] >= 48 && Char.code s.[i] <= 57 then
        int_of_char s.[i] - 48
      else h f stop_n (f i)
    in
    if rev then h (fun n -> n - 1) (-1) (String.length s - 1)
    else h (( + ) 1) (String.length s) 0

  let run (input : string) : (string, string) result =
    String.split_on_char '\n' input
    |> List.fold_left
         (fun acc s ->
           let r1 = fst_digit s in
           let r2 = fst_digit ~rev:true s in
           acc + (r1 * 10) + r2)
         0
    |> string_of_int |> Result.ok
end

module Part_2 = struct
  let hs =
    let t = Hashtbl.create 1 in
    Hashtbl.add t "one" 1;
    Hashtbl.add t "two" 2;
    Hashtbl.add t "three" 3;
    Hashtbl.add t "four" 4;
    Hashtbl.add t "five" 5;
    Hashtbl.add t "six" 6;
    Hashtbl.add t "seven" 7;
    Hashtbl.add t "eight" 8;
    Hashtbl.add t "nine" 9;
    t

  let r =
    Hashtbl.fold (fun k _ acc -> acc ^ Format.sprintf "%s\\|" k) hs ""
    |> (fun a -> a ^ "[0-9]")
    |> Str.regexp

  let matched s =
    let matched = Str.matched_string s in
    match Hashtbl.find_opt hs matched with
    | Some v -> v
    | None -> matched |> int_of_string

  let run (input : string) : (string, string) result =
    String.split_on_char '\n' input
    |> List.fold_left
         (fun acc s ->
           if s = "" then acc
           else (
             Str.search_forward r s 0 |> ignore;
             let r1 = matched s in
             Str.search_backward r s (String.length s - 1) |> ignore;
             let r2 = matched s in
             acc + (r1 * 10) + r2))
         0
    |> string_of_int |> Result.ok
end
