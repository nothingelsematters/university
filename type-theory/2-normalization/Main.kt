import lambda.*
import java.util.Scanner

fun main() {
    with (Scanner(System.`in`).useDelimiter("\\A")) {
        print(Expression(next()))
    }
}
