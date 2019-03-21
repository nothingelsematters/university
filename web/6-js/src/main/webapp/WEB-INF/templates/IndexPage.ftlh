<#-- @ftlvariable name="message" type="java.lang.String" -->

<#import "common.ftl" as c>

<@c.page>
    <#if message??>
        <p>${message}</p>
    </#if>
<div class="articles">
    <div class="no-data">No data</div>
</div>

<template class="article-template">
    <article>
        <div class="title">{{title}}</div>
        <div class="information">by {{login}}</div>
        <div class="body">
            <p>{{text}}</p>
        </div>
        <div class="footer">
            <div class="left">
                <img src="img/voteup.png" title="Vote Up" alt="Vote Up"/>
                <span class="positive-score">+173</span>
                <img src="img/votedown.png" title="Vote Down" alt="Vote Down"/>
            </div>
            <div class="right">
                <img src="img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                <div class="time">{{time}}</div>
                <img src="img/comments_16x16.png" title="Comments" alt="Comments"/>
                <a href="#">68</a>
            </div>
        </div>
    </article>
</template>

<script>
    $(function () {
        var $noData = $(".articles .no-data");
        var $articles = $(".articles");
        $noData.text("Loading...");
        $.post("", {action: "find"}, function (articles) {
            if (articles.length > 0) {
                $noData.empty();
                articles.forEach(function (article) {
                    var $tr = cloneTemplate($("template.article-template"));
                    $tr.find(".title").text(article.title);
                    $tr.find(".information").text("by " + article.userName);
                    $tr.find("p").text(article.text);
                    $tr.find(".time").text(article.creationTime);
                    $articles.append($tr);
                });
            } else {
                $noData.text("No data");
            }
        });
    });
</script>
</@c.page>
