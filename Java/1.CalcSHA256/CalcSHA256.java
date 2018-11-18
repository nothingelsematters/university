import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.MessageDigest;

public class CalcSHA256 {
    public static void main(String[] args) throws Exception {
        for (String line : Files.readAllLines(Paths.get(args[0]))) {
            for (byte hash : MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(Paths.get(line)))) {
                System.out.printf("%02X", hash);
            }
            System.out.println();
        }
    }
}
