let scprintf color s =
        Printf.ksprintf (fun s ->
                sprintf "\x1b[38;2;%sm%s\x1b[0m" color s 
        )

let cprintf color = Printf.kprintf(fun s ->
        printf "\x1b[38;2;%sm%s\x1b[0m" color s 
)

let cprintfn color = Printf.kprintf(fun s ->
        printfn "\x1b[38;2;%sm%s\x1b[0m" color s 
)

module PColors =
    let gray = "128;128;128"
    let green = "0;255;0"
    let orange = "255;128;0"
