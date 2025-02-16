#r "nuget: Selenium.WebDriver"
#r "nuget: canopy"

open System
open canopy
open canopy.classic
open OpenQA.Selenium

configuration.chromeDir <- @"d:\dev\tools"
let champignyUrl = Environment.GetEnvironmentVariable "TRANSMISSION_CHAMPIGNY_URL"

let startMode = 
            let localAppData = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData)
            let chromeData = IO.Path.Combine(localAppData, @"Google\Chrome\User Data")
            let options = Chrome.ChromeOptions()
            options.AddArgument($"user-data-dir={chromeData}")
            let start = canopy.types.BrowserStartMode.ChromeWithOptions options
            start
        
configuration.compareTimeout <- 20.

let updateMarkdown docUrl file =
    let isClosed =
        if browser = null then
            true
        else
            try 
                browser.CurrentWindowHandle = null 
            with
            | _ -> true
    if isClosed then
        start startMode

    if currentUrl() <> docUrl   then
        url docUrl

    if someElement "#text" = None then
        click "Extensions"
        hover "Docs™ to Markdown"
        click "Convert"
        check "#italic_bold_underscores"
        check "#reckless_mode"

    click "Markdown"
    waitFor (fun () -> (read "#text").StartsWith("Converting text") |> not  )
    let text = read "#text" 
    System.IO.File.WriteAllText(file,text)
