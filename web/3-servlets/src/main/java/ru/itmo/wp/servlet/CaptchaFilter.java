package ru.itmo.wp.servlet;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Base64;
import java.util.concurrent.ThreadLocalRandom;

import ru.itmo.wp.util.ImageUtils;

public class CaptchaFilter extends HttpFilter {
    private static String makeCaptcha(Integer num, String redirectTo) {
        String captchaPng = Base64.getEncoder().encodeToString(ImageUtils.toPng(num.toString()));
        String[] html = {"<!DOCTYPE HTML>\n" +
                "<html lang=\"en\">\n" +
                "<head>\n" +
                "    <meta charset=\"UTF-8\">\n" +
                "    <title>Captcha</title>\n" +
                "</head>\n" +
                "<body>\n" +
                "    <div class=\"captcha-form\">\n" +
                "        <img src=\"data:image/png;base64, ",

                "\">\n" +
                "        <form class =\"captcha-form\" method=\"get\" action=\"",

                "\">\n" +
                "           <label for=\"captcha_answer\">Enter answer:</label>\n" +
                "            <input name=\"captcha\" id=\"captcha_answer\">\n" +
                "        </form>\n" +
                "    </div>\n" +
                "</html>"};
        return html[0] + captchaPng + html[1] + redirectTo + html[2];
    }


    @Override
    protected void doFilter(HttpServletRequest request,
                            HttpServletResponse response,
                            FilterChain chain)
            throws IOException, ServletException {
        HttpSession session = request.getSession();
        if ("true".equals(session.getAttribute("captcha-accepted"))) {
            chain.doFilter(request, response);
            return;
        }

        String answer = request.getParameter("captcha");
        if (answer != null && answer.equals(session.getAttribute("Expected-Answer"))) {
            session.removeAttribute("Expected-Answer");
            session.setAttribute("captcha-accepted", "true");
            response.sendRedirect(request.getRequestURI());
            chain.doFilter(request, response);
            return;
        }

        int randomNum = ThreadLocalRandom.current().nextInt(100, 1000);
        session.setAttribute("Expected-Answer", Integer.toString(randomNum));

        response.setContentType("text/html");
        OutputStream outputStream = response.getOutputStream();
        outputStream.write(makeCaptcha(randomNum, request.getRequestURI()).getBytes());
        outputStream.flush();
    }
}
