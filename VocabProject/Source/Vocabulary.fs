
namespace Jmas.Vocabulary

open System
open System.IO
open System.Text

module VocabProject =
    type VocabEntryDialect =
        | British
        | American
        | Australian
    type VocabEntryLinguisticOrigin =
        | French
        | Latin
        | Italian
        | Greek
        | Spanish
        | Japanese
    type VocabEntryGeometryOrigin =
        | Mexican
    type VocabEntryBiologySubject =
        | Biology
        | Botany
        | Biochemical
        | Genetics
        | Anatomy
    type VocabEntrySubject =
        | Mathematics
        | Physics
        | Chemical
        | ComputerScience
        | Biology of p: VocabEntryBiologySubject
    type VocabEntryProperty =
        | Dialect of p: VocabEntryDialect
        | Linguistics of p: VocabEntryLinguisticOrigin
        | Geometry of p: VocabEntryGeometryOrigin
        | Subject of p: VocabEntrySubject
        | Corrupted
        static member ConvertFromString (input: string) =
            match input.ToLower() with
            | "british" -> Dialect British
            | "american" -> Dialect American
            | "australian" -> Dialect Australian
            | "french"
            | "français" -> Linguistics French
            | "latin" -> Linguistics Latin
            | "italian" -> Linguistics Italian
            | "greek" -> Linguistics Greek
            | "spanish"
            | "español" -> Linguistics Spanish
            | "japanese" -> Linguistics Japanese
            | "mexican" -> Geometry Mexican
            | "mathematics" -> Subject Mathematics
            | "physics"
            | "physical" -> Subject Physics
            | "chemistry"
            | "chemical" -> Subject Chemical
            | "biology"
            | "biological" -> Subject (Biology VocabEntryBiologySubject.Biology)
            | "botany" -> Subject (Biology Botany)
            | "biochemical"
            | "biochemistry" -> Subject (Biology Biochemical)
            | "genetics" -> Subject (Biology Genetics)
            | "anatomy" -> Subject (Biology Anatomy)
            | "cs" -> Subject ComputerScience
            | _ -> Corrupted
        static member ConvertToIntFromString (input: string) =
            match input.ToLower() with
            | "british" -> 0x100
            | "american" -> 0x101
            | "australian" -> 0x102
            | "french"
            | "français" -> 0x200
            | "latin" -> 0x201
            | "italian" -> 0x202
            | "greek" -> 0x203
            | "spanish"
            | "español" -> 0x204
            | "japanese" -> 0x205
            | "mexican" -> 0x300
            | "mathematics" -> 0x400
            | "physics"
            | "physical" -> 0x401
            | "chemistry"
            | "chemical" -> 0x402
            | "biology"
            | "biological" -> 0x410
            | "botany" -> 0x411
            | "biochemical"
            | "biochemistry" -> 0x412
            | "genetics" -> 0x413
            | "anatomy" -> 0x414
            | "cs" -> 0x403
            | _ -> 0
        static member IsBiologySubject attr =
            attr &&& 0x410 <> 0
    type internal State = WordCount | TabCount | OneLessThan | TwoLessThan | TwoLessOneGreater | Explanation_Plain | ErrorState
    type ConstructVocabEntryResult =
        | Success of entry: VocabEntry
        | Error of errorMessage: string
    and VocabEntry(word: string, attributeList: int list, explanation: string, lineNumber: int) =
        member val Word = word
        member val AttributeList = attributeList
        member val Explanation = explanation
        member val LineNumber = lineNumber
        member this.ContainsAttribute attr =
            let rec p attr attrList =
                match attrList with
                | head::tail ->
                    if head = attr then true
                    else p attr tail
                | [] -> false
            p attr this.AttributeList
        member this.HasBiologyAttribute () =
            let rec p attrList =
                match attrList with
                | head::tail ->
                    if VocabEntryProperty.IsBiologySubject head then true
                    else p tail
                | [] -> false
            p this.AttributeList
        static member getTabCount wcc =
            if wcc < 16 then 4-wcc/4
            else 2
        static member checkTabCount wcc tc =
            tc = VocabEntry.getTabCount wcc
        static member Construct lineNumber (line: string) =
            let mutable state = WordCount
            let mutable wordCharacterCount=0
            let mutable tabCount=0
            let mutable attributeList = []
            let mutable attributeSeq = StringBuilder()
            let mutable explanationBegin = -1
            let mutable errorMessage:string = null

            let toError charIndex message =
                errorMessage <- sprintf "line %d column %d (%c): error: %s" lineNumber charIndex line.[charIndex] message
                state <- ErrorState
            let readChar index =
                let ch = line.[index]
                match state with
                | WordCount ->
                    match ch with
                    | ' '
                    | '-'
                    | '('
                    | ')'
                    | ''' ->
                        wordCharacterCount <- wordCharacterCount + 1
                    | letter when Char.IsLetterOrDigit letter ->
                        wordCharacterCount <- wordCharacterCount + 1
                    | '\t' ->
                        state <- TabCount
                        tabCount <- 1
                    | _ ->
                        toError index "' ', '\t', other character expected."
                | TabCount ->
                    match ch with
                    | '\t' ->
                        tabCount <- tabCount + 1
                    | '<' ->
                        state <- OneLessThan
                        explanationBegin <- index
                    | _ ->
                        state <- Explanation_Plain
                        explanationBegin <- index
                | Explanation_Plain ->
                    match ch with
                    | '<' ->
                        state <- OneLessThan
                    | _ ->
                        state <- Explanation_Plain
                | OneLessThan ->
                    match ch with
                    | '<' ->
                        state <- TwoLessThan
                    | _ ->
                        state <- Explanation_Plain
                | TwoLessThan ->
                    match ch with
                    | '>' ->
                        attributeList <- attributeList @ [VocabEntryProperty.ConvertToIntFromString (attributeSeq.ToString())]
                        state <- TwoLessOneGreater
                        attributeSeq <- attributeSeq.Clear()
                    | _ ->
                        attributeSeq <- attributeSeq.Append(ch)
                | TwoLessOneGreater ->
                    match ch with
                    | '>' ->
                        state <- Explanation_Plain
                    | _ ->
                        errorMessage <- "Error: '>' expected."
                        state <- ErrorState
                | ErrorState -> ()
            let endOfLine () =
                match state with
                | Explanation_Plain -> ()
                | TabCount -> ()
                | WordCount ->
                    toError (line.Length-1) "'\\t' expected."
                | OneLessThan ->
                    toError (line.Length-1) "'<' or other character expected."
                | TwoLessThan ->
                    toError (line.Length-1) "other character expected."
                | TwoLessOneGreater ->
                    toError (line.Length-1) "'>' expected."
                | ErrorState -> ()

            if line.Length <> 0 then
                for i = 0 to line.Length-1 do
                    readChar i
                endOfLine ()
                if errorMessage <> null then
                    Error (sprintf "line %d: %s" lineNumber errorMessage)
                elif VocabEntry.checkTabCount wordCharacterCount tabCount = false then
                    Error (sprintf "line %d: Tab count mismatch, %d expected, %d got." lineNumber (VocabEntry.getTabCount wordCharacterCount) tabCount)
                else
                    let explanation = if explanationBegin = -1 then String.Empty else line.Substring(explanationBegin)
                    Success (VocabEntry(line.Substring(0, wordCharacterCount), attributeList, explanation, lineNumber))
            else
                Error "Empty line"
        member this.ToString_WordWithTab () =
            let rec acc times s =
                match times with
                | 0 -> s
                | _ -> acc (times-1) (s+"\t")
            acc (VocabEntry.getTabCount this.Word.Length) this.Word
        member this.WordAndExplanation () = this.ToString_WordWithTab () + this.Explanation
        static member CompareWord (a: VocabEntry) (b: VocabEntry) =
            String.Compare(a.Word, b.Word)
//    type VocabEntryFilter =
//        abstract member Filter: VocabEntry->bool
//        abstract member Sort: VocabEntry->VocabEntry->int
//        abstract member Format: VocabEntry->string
//    let limitNumber number (filter: VocabEntry -> bool) (sort: VocabEntry -> VocabEntry -> int) (format: VocabEntry -> string) =
//        { new VocabEntryFilter with
//            member this.Filter a = filter a
//            member this.Sort a b = sort a b
//            member this.Format a = format a
//        }

    let ReadEntriesFromFile fileName =
        let allLines = File.ReadAllLines fileName
        let wordLinesWithLineNumber = allLines |> Array.mapi (fun index s -> (index, s)) |> Array.filter (fun (_, g) -> g |> (String.IsNullOrEmpty >> not))
        let readResult = Array.map (fun (lineNumber, line) -> VocabEntry.Construct lineNumber line) wordLinesWithLineNumber
        let isError t =
            match t with
            | Error _ -> true
            | Success _ -> false
        let (errors, entries) = Array.partition isError readResult
        let printErrorResult t =
            match t with
            | Error message -> printfn "%s" message
            | _ -> ()
        Array.iter printErrorResult errors
        let g p =
            match p with
            | Success entry -> entry
            | Error _ -> failwith "anything"
        entries |> Array.map g

    let PrintEntriesToFile (newFileName: string) (filter: VocabEntry -> bool) (numberLimit: int) (sort: VocabEntry -> VocabEntry -> int) (format: VocabEntry -> string) (entries: VocabEntry array) =
        let stream = new StreamWriter(newFileName)
        let r = Random()
        let wordsBeforePick = entries |> Array.filter filter |> Array.sortBy (fun _ -> r.Next())
        let wordsToWrite =
            if numberLimit <= wordsBeforePick.Length then Array.sub wordsBeforePick 0 numberLimit
            else wordsBeforePick
        let singleLine = wordsToWrite |> Array.sortWith sort |> Array.map format |> Array.fold (fun a b -> if String.IsNullOrEmpty a then b else a+"\n"+b) ""
        let writeToFileTask = stream.WriteLineAsync singleLine
        let task1 = writeToFileTask.ContinueWith (fun _ -> stream.Close())
        let singleLineWithAns = wordsToWrite |> Array.sortWith sort |> Array.map (fun a -> a.WordAndExplanation ()) |> Array.fold (fun a b -> if String.IsNullOrEmpty a then b else a + "\n" + b) ""
        let streamAns = new StreamWriter (if newFileName.Length > 4 then newFileName.Substring(0, newFileName.Length - 4) + " Answer.txt" else "Answer.txt")
        let writeAnsToFileTask = streamAns.WriteLineAsync singleLineWithAns
        let task2 = writeAnsToFileTask.ContinueWith (fun _ -> streamAns.Close())
        System.Threading.Tasks.Task.WhenAll(task1, task2)

    let PrintSelectedEntriesToFile (newFileName: string) (filter: VocabEntry -> bool) (fromWord: string) (toWord: string) (numberLimit: int) (sort: VocabEntry -> VocabEntry -> int) (format: VocabEntry -> string) (entries: VocabEntry array) =
        let fromIndex = Array.findIndex (fun (t: VocabEntry) -> t.Word = fromWord) entries
        let toIndex = Array.findIndex (fun (t: VocabEntry) -> t.Word = toWord) entries
        let fromToRange (a: VocabEntry) = a.LineNumber <= toIndex && a.LineNumber >= fromIndex
        let f a = (filter a) && (fromToRange a)
        let stream = new StreamWriter(newFileName)
        let r = Random()
        let wordsBeforePick = entries |> Array.filter f |> Array.sortBy (fun _ -> r.Next())
        let wordsToWrite =
            if numberLimit <= wordsBeforePick.Length then Array.sub wordsBeforePick 0 numberLimit
            else wordsBeforePick
        let singleLine = wordsToWrite |> Array.sortWith sort |> Array.map format |> Array.fold (fun a b -> if String.IsNullOrEmpty a then b else a+"\n"+b) ""
        let writeToFileTask = stream.WriteLineAsync singleLine
        let task1 = writeToFileTask.ContinueWith (fun _ -> stream.Close())
        let singleLineWithAns = wordsToWrite |> Array.sortWith sort |> Array.map (fun a -> a.WordAndExplanation ()) |> Array.fold (fun a b -> if String.IsNullOrEmpty a then b else a + "\n" + b) ""
        let streamAns = new StreamWriter (if newFileName.Length > 4 then newFileName.Substring(0, newFileName.Length - 4) + " Answer.txt" else "Answer.txt")
        let writeAnsToFileTask = streamAns.WriteLineAsync singleLineWithAns
        let task2 = writeAnsToFileTask.ContinueWith (fun _ -> streamAns.Close())
        System.Threading.Tasks.Task.WhenAll(task1, task2)

    let WorkOnDirectory (directory:string) (fileName: string) (newFileName: string) filter numberLimit sort format fromWord toWord =
        ReadEntriesFromFile (Path.Combine(directory, fileName)) |> PrintSelectedEntriesToFile (Path.Combine(directory, newFileName)) filter fromWord toWord numberLimit sort format
        
    let HardWordsFromVocabTest () =
        let a = WorkOnDirectory (Path.Combine("E:", "Aaron", "Vocab")) "Vocab11_2019.txt" "GeneratedWordList\\hard 2021.1.1.txt"
        a (fun entry -> not (entry.Word.Contains(' ')) && entry.Explanation.Length > 3) 1000 VocabEntry.CompareWord (fun a -> a.ToString_WordWithTab()) "evangelize" "fledgling"

    let LatestWords () =
        let f (entry: VocabEntry) = entry.Word.Length <= 16 && not (entry.HasBiologyAttribute ()) && not (entry.Word.Contains(' ')) && entry.Explanation.Length > 3
        let f2 (entry: VocabEntry) = (f entry) && entry.LineNumber > 4000
        let a = WorkOnDirectory (Path.Combine("E:","Aaron","Vocab")) "Vocab11_2019.txt" "GeneratedWordList\\latest 2021.1.1.txt"
        a f 100 VocabEntry.CompareWord (fun a -> a.ToString_WordWithTab()) "sealioning" "espiocrat"
        
