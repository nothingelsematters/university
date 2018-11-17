<#-- @ftlvariable name="users" type="ru.itmo.webmail.model.domain.User[]" -->
<#import "common.ftl" as c>

<@c.page>
<div class="datatable my-articles">
    <div class="caption">Articles</div>
    <table>
        <thead>
        <tr>
            <th>#</th>
            <th>Time</th>
            <th>Title</th>
            <th>Display</th>
        </tr>
        </thead>
        <tbody>
            <#list articles as article>
                <tr>

                        <td class="id">${article.id}</td>
                        <td class="time">${article.creationTime}</td>
                        <td class="title">${article.title}</td>
                        <td>
                            <form method="post" action="/myArticles">
                            <input type="hidden" name="action" value="change">
                            <#if article.hidden>
                                <input type="submit" class="main" value="Show">
                            <#else>
                                <input type="submit" class="main" value="Hide">
                            </#if>
                            <div class="error"></div>
                            </form>
                        </td>
                </tr>
            </#list>
        </tbody>
    </table>
</div>

<script>
    $(function() {
        $("form").submit(function() {
            var $tr = $(this).closest("tr");
            var articleId = $tr.find(".id").text();
            var $button = $(this).find(".main");
            var changing = $button.attr("value");
            var $error = $tr.find(".error");

            $.post("", {action: "change", articleId: articleId, changing: changing}, function (response) {
                if (response["success"]) {
                    $button.attr("value", (changing == "Show" ? "Hide" : "Show"));
                } else {
                    $error.text(response["error"]);
                }
            });
            return false;
        });
    });
</script>

</@c.page>
