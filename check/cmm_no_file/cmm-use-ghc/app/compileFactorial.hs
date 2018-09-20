import CompileParsed
import ParseCmmFactorial

main :: IO ()
main = compileCmmGroup "factorial" myCmmFactorial0
