package com.github.nothingelsematters.sd.refactoring.http;

import com.github.nothingelsematters.sd.refactoring.entity.Product;

import java.util.List;

public interface HttpFormatter {

    String contentPage(String content);

    String headerContent(String header, String content);

    String productList(List<Product> products);

    String errorPage(String errorMessage);
}
