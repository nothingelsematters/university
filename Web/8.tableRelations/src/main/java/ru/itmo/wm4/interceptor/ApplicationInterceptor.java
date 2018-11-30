package ru.itmo.wm4.interceptor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;
import ru.itmo.wm4.controller.IndexPage;
import ru.itmo.wm4.controller.Page;
import ru.itmo.wm4.domain.Role;
import ru.itmo.wm4.domain.User;
import ru.itmo.wm4.security.AnyRole;
import ru.itmo.wm4.security.Guest;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.lang.reflect.Method;

@Component
public class ApplicationInterceptor implements HandlerInterceptor {
    private IndexPage indexPage;

    @Autowired
    public void setIndexPage(IndexPage indexPage) {
        this.indexPage = indexPage;
    }

    @Override
    public boolean preHandle(
            HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
        if (handler instanceof HandlerMethod) {
            HandlerMethod handlerMethod = (HandlerMethod) handler;
            Method method = handlerMethod.getMethod();
            Boolean redirected = false;

            if (Page.class.isAssignableFrom(method.getDeclaringClass())) {
                if (method.getAnnotation(Guest.class) != null) {
                    return true;
                }

                AnyRole anyRole = method.getAnnotation(AnyRole.class);
                if (anyRole != null) {
                    User user = indexPage.getUser(request.getSession());
                    if (user != null) {
                        for (Role role : user.getRoles()) {
                            for (Role.Name name : anyRole.value()) {
                                if (role.getName() == name) {
                                    return true;
                                }
                            }
                        }
                        response.sendRedirect("/");
                        redirected = true;
                    }
                }

                if (!redirected) {
                    response.sendRedirect("/enter");
                }
                return false;
            }
        }
        return true;
    }

    @Override
    public void postHandle(
            HttpServletRequest request, HttpServletResponse response, Object handler,
            ModelAndView modelAndView) {
        // No operations.
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response,
                                Object handler, Exception exception) {
        // No operations.
    }
}