<#import "common.ftl" as c>

<@c.page>
<div class="add-article form-box">
    <div class="header">Add Article</div>
    <div class="body">
        <form method="post" action="/article">
            <input type="hidden" name="action" value="article">
            <div class="field">
                <div class="name">
                    <label for="title">Title</label>
                </div>
                <div class="value">
                    <input id="title" name="title"/>
                </div>
            </div>
            <div class="field">
                <div class="name">
                    <label for="text">Text</label>
                </div>
                <div class="value">
                    <textarea id="text" name="text"></textarea>
                </div>
            </div>
            <div class="field">
                <div class="name">
                    <label for="hidden">Hidden</label>
                </div>
                <div class="value">
                    <input type="checkbox" id="hidden" name="hidden"/>
                </div>
            </div>
            <div class="button-field">
                <input type="submit" value="Create">
            </div>
            <div class="error"></div>
        </form>
    </div>
</div>

<script>
    $(function() {
        $(".add-article form").submit(function () {
            var title = $(this).find("input[name='title']").val();
            var text = $(this).find("textarea[name='text']").val();
            var hidden = $(this).find("input[name='hidden']").prop("checked");
            var $error = $(this).find(".error").text("");
            $.post("", {action: "article", title: title, text: text, hidden: hidden}, function (response) {
                if (response["success"]) {
                    setMessage("Article Added!");
                    document.location.href = "/";
                } else {
                    $error.text(response["error"]);
                }
            });
            return false;
        });
    })
</script>
</@c.page>