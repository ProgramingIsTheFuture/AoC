let year = 2023
let day = 2

module Part_1 = struct
  let maxs = [ ("green", 13); ("red", 12); ("blue", 14) ]
  let is_valid k v = List.assoc k maxs >= v

  let check_all_valid a =
    String.split_on_char ',' a
    |> List.fold_left
         (fun acc a ->
           let a = String.trim a in
           let n, k = Scanf.sscanf a "%d %s" (fun n k -> (n, k)) in
           is_valid k n && acc)
         true

  let run (input : string) : (string, string) result =
    String.split_on_char '\n' input
    |> List.fold_left
         (fun acc s ->
           if s = "" then acc
           else
             let id = Scanf.sscanf s "Game %d: " (fun id -> id) in
             let s =
               String.sub s
                 (7 + (string_of_int id |> String.length))
                 (String.length s - (7 + (string_of_int id |> String.length)))
             in
             let s = String.split_on_char ';' s in
             if List.fold_left (fun acc a -> check_all_valid a && acc) true s
             then acc + id
             else acc)
         0
    |> string_of_int |> Result.ok
end

module Part_2 = struct
  let power_set_all a =
    let r, g, b =
      List.fold_left
        (fun acc a ->
          String.split_on_char ',' a
          |> List.fold_left
               (fun (r, g, b) a ->
                 let a = String.trim a in
                 let n, k = Scanf.sscanf a "%d %s" (fun n k -> (n, k)) in
                 match k with
                 | "red" when n > r -> (n, g, b)
                 | "green" when n > g -> (r, n, b)
                 | "blue" when n > b -> (r, g, n)
                 | _ -> (r, g, b))
               acc)
        (0, 0, 0) a
    in
    r * g * b

  let run (input : string) : (string, string) result =
    String.split_on_char '\n' input
    |> List.fold_left
         (fun acc s ->
           if s = "" then acc
           else
             let id = Scanf.sscanf s "Game %d: " (fun id -> id) in
             let s =
               String.sub s
                 (7 + (string_of_int id |> String.length))
                 (String.length s - (7 + (string_of_int id |> String.length)))
             in
             let s = String.split_on_char ';' s in
             power_set_all s + acc)
         0
    |> string_of_int |> Result.ok
end
