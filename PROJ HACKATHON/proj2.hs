import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import Data.List (nub)
import Data.Char (toLower)
import qualified Data.Map as M

type UserName = String
type ClassCode = String
type Session = [(UserName, ClassCode)]
type Assignment = (UserName, ClassCode, String)
type ChatMessage = (UserName, ClassCode, String)
type QuizResult = (UserName, ClassCode, Int, String)

main :: IO ()
main = do
    putStrLn "Welcome to the Virtual Classroom!"
    mainMenu [] [] [] []

mainMenu :: Session -> [Assignment] -> [ChatMessage] -> [QuizResult] -> IO ()
mainMenu session assignments chats quizResults = do
    putStrLn "\nMenu:"
    putStrLn "1. Create Class"
    putStrLn "2. Join Class"
    putStrLn "3. View Dashboard"
    putStrLn "4. Submit Assignment (File)"
    putStrLn "5. Class Chat"
    putStrLn "6. Take Quiz"
    putStrLn "7. Exit"
    putStr "Enter your choice: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> createClass session assignments chats quizResults
        "2" -> joinClass session assignments chats quizResults
        "3" -> dashboard session assignments chats quizResults
        "4" -> submitAssignment session assignments chats quizResults
        "5" -> classChat session assignments chats quizResults
        "6" -> takeQuiz session assignments chats quizResults
        "7" -> putStrLn "Goodbye!"
        _   -> putStrLn "Invalid choice." >> mainMenu session assignments chats quizResults

createClass :: Session -> [Assignment] -> [ChatMessage] -> [QuizResult] -> IO ()
createClass session assignments chats quizResults = do
    putStr "Enter your name: "
    hFlush stdout
    name <- getLine
    putStr "Enter a new class code: "
    hFlush stdout
    code <- getLine
    putStrLn $ "[OK] Class '" ++ code ++ "' created by " ++ name
    mainMenu ((name, code):session) assignments chats quizResults

joinClass :: Session -> [Assignment] -> [ChatMessage] -> [QuizResult] -> IO ()
joinClass session assignments chats quizResults = do
    putStr "Enter your name: "
    hFlush stdout
    name <- getLine
    putStr "Enter class code to join: "
    hFlush stdout
    code <- getLine
    putStrLn $ "[OK] " ++ name ++ " joined class '" ++ code ++ "'"
    mainMenu ((name, code):session) assignments chats quizResults

dashboard :: Session -> [Assignment] -> [ChatMessage] -> [QuizResult] -> IO ()
dashboard session _ _ quizResults = do
    putStrLn "\n Class Dashboard (Grouped by Subject):"
    let grouped = groupByClass session quizResults
    if null session
        then putStrLn "[!] No users have joined any classes yet."
        else mapM_ printClass (M.toList grouped)
    writeHtmlDashboard grouped
    mainMenu session [] [] quizResults


groupByClass :: Session -> [QuizResult] -> M.Map ClassCode [(UserName, Int, String)]
groupByClass session quizResults =
    let usersByClass = M.fromListWith (++) [ (c, [u]) | (u, c) <- session ]
        resultsMap = M.fromListWith (++) [ ((u, c), [(s, g)]) | (u, c, s, g) <- quizResults ]
    in M.mapWithKey (\c users ->
            [ (u, s, g)
            | u <- users
            , let (s, g) = case M.lookup (u, c) resultsMap of
                                Just ((s', g'):_) -> (s', g')
                                Nothing -> (0, "N/A")
            ]) usersByClass

printClass :: (ClassCode, [(UserName, Int, String)]) -> IO ()
printClass (code, students) = do
    putStrLn $ "\nClass: " ++ code
    if null students
        then putStrLn "  No students."
        else mapM_ (\(u, s, g) -> putStrLn $ "  - " ++ u ++ ": Score = " ++ show s ++ ", Grade = " ++ g) students

submitAssignment :: Session -> [Assignment] -> [ChatMessage] -> [QuizResult] -> IO ()
submitAssignment session assignments chats quizResults = do
    putStr "Enter path to File 1: "
    hFlush stdout
    file1 <- getLine
    putStr "Enter path to File 2: "
    hFlush stdout
    file2 <- getLine
    exists1 <- doesFileExist file1
    exists2 <- doesFileExist file2
    if not exists1 || not exists2
        then putStrLn "[X] One or both files not found."
        else do
            content1 <- readFile file1
            content2 <- readFile file2
            let sim = similarity content1 content2
            putStrLn $ "[OK] Similarity: " ++ show (sim * 100) ++ "%"
            if sim > 0.8
                then putStrLn "[!] Plagiarism likely detected."
                else putStrLn "[V] Files seem original."
    mainMenu session assignments chats quizResults

similarity :: String -> String -> Double
similarity a b =
    let wsA = words a
        wsB = words b
        common = length $ filter (`elem` wsB) wsA
        total = max (length (nub wsA)) 1
    in fromIntegral common / fromIntegral total

classChat :: Session -> [Assignment] -> [ChatMessage] -> [QuizResult] -> IO ()
classChat session assignments chats quizResults = do
    putStrLn "\n--- Class Chat ---"
    mapM_ (\(u, c, m) -> putStrLn $ "[" ++ c ++ "] " ++ u ++ ": " ++ m) (reverse chats)
    putStr "\nDo you want to post a message? (yes/no): "
    hFlush stdout
    answer <- getLine
    if answer == "yes"
        then do
            putStr "Your name: "
            hFlush stdout
            name <- getLine
            putStr "Class code: "
            hFlush stdout
            code <- getLine
            putStr "Enter message: "
            hFlush stdout
            msg <- getLine
            putStrLn "[OK] Message posted!"
            let userMessage = (name, code, msg)
            let botReply = ("ClassBot", code, generateBotReply msg)
            mainMenu session assignments (botReply : userMessage : chats) quizResults
        else mainMenu session assignments chats quizResults

generateBotReply :: String -> String
generateBotReply msg
    | "hello" `elem` lw = "Hello! How can I assist you today?"
    | "assignment" `elem` lw = "Need help with the assignment? Make sure to submit it before the deadline!"
    | "help" `elem` lw = "Sure! Here's help: https://www.google.com https://www.stackoverflow.com"
    | "bye" `elem` lw || "exit" `elem` lw = "Goodbye! Keep learning."
    | otherwise = "Thanks for your message. Let me know if you need help."
  where lw = words (map toLower msg)

takeQuiz :: Session -> [Assignment] -> [ChatMessage] -> [QuizResult] -> IO ()
takeQuiz session assignments chats quizResults = do
    putStr "Enter your name: "
    hFlush stdout
    name <- getLine
    putStr "Enter your class code: "
    hFlush stdout
    code <- getLine
    putStrLn "\n--- Quiz Time! ---"
    score1 <- askMCQ "1. Capital of France?" [("a", "Paris"), ("b", "Rome"), ("c", "Berlin")] "a"
    score2 <- askMCQ "2. Language for system programming?" [("a", "HTML"), ("b", "C"), ("c", "CSS")] "b"
    score3 <- askMCQ "3. CPU stands for?" [("a", "Central Processing Unit"), ("b", "Control Unit"), ("c", "Computer Power Unit")] "a"
    let total = score1 + score2 + score3
    let gradeStr = grade total
    putStrLn $ "\nYour score: " ++ show total ++ " / 3"
    putStrLn $ "Grade: " ++ gradeStr
    let newResult = (name, code, total, gradeStr)
    mainMenu session assignments chats (newResult : quizResults)

askMCQ :: String -> [(String, String)] -> String -> IO Int
askMCQ question options correct = do
    putStrLn question
    mapM_ (\(k, v) -> putStrLn $ k ++ ") " ++ v) options
    putStr "Your answer: "
    hFlush stdout
    ans <- getLine
    if map toLower ans == correct
        then putStrLn "[V] Correct!" >> return 1
        else putStrLn "[X] Incorrect." >> return 0

grade :: Int -> String
grade 3 = "A"
grade 2 = "B"
grade 1 = "C"
grade _ = "F"

-- HTML dashboard
writeHtmlDashboard :: M.Map ClassCode [(UserName, Int, String)] -> IO ()
writeHtmlDashboard grouped = do
    let htmlHeader = "<!DOCTYPE html>\n<html>\n<head>\n<title>Class Dashboard</title>\n<style>\nbody { font-family: Arial; background: #f8f8f8; padding: 20px; }\n.class-box { background: #fff; border: 1px solid #ddd; border-radius: 8px; padding: 10px; margin-bottom: 15px; box-shadow: 2px 2px 5px #ccc; }\nh2 { color: #2c3e50; }\nul { list-style-type: none; padding-left: 10px; }\nli { margin: 4px 0; }\n</style>\n</head>\n<body>\n<h1>Class Dashboard</h1>\n"
        htmlFooter = "\n</body>\n</html>"
        classSections = concatMap classToHtml (M.toList grouped)
    writeFile "output.html" (htmlHeader ++ classSections ++ htmlFooter)
    putStrLn "[V] HTML dashboard exported to output.html"

classToHtml :: (ClassCode, [(UserName, Int, String)]) -> String
classToHtml (code, students) =
    "<div class=\"class-box\">\n<h2>Class: " ++ code ++ "</h2>\n" ++
    (if null students
        then "<p>No students.</p>\n"
        else "<ul>\n" ++ concatMap studentToLi students ++ "</ul>\n") ++
    "</div>\n"

studentToLi :: (UserName, Int, String) -> String
studentToLi (u, s, g) = "<li><strong>" ++ u ++ "</strong>: Score = " ++ show s ++ ", Grade = " ++ g ++ "</li>\n"
