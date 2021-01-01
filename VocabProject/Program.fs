namespace Jmas.Vocabulary
module VocabProgram =

    open System
    [<EntryPoint>]
    let main argv =
        let task1 = VocabProject.LatestWords()
        let task2 = VocabProject.HardWordsFromVocabTest()
        task1.Wait ()
        task2.Wait ()
        0 // return an integer exit code