package ru.itmo.wm4;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import ru.itmo.wm4.interceptor.ApplicationInterceptor;

@SpringBootApplication
public class Wm4Application implements WebMvcConfigurer {
    private ApplicationInterceptor applicationInterceptor;

    @Autowired
    public void setApplicationInterceptor(ApplicationInterceptor applicationInterceptor) {
        this.applicationInterceptor = applicationInterceptor;
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(applicationInterceptor);
    }

    public static void main(String[] args) {
        SpringApplication.run(Wm4Application.class, args);
    }
}
