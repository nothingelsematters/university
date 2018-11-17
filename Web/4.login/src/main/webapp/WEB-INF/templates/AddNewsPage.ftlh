<#import "common.ftl" as c>

<@c.page>
<div class="form-box">
    <div class="header">Add News</div>
    <div class="body">
        <form method="post" action="/addNews">
            <input type="hidden" name="action" value="addNews">
            <div class="field">
                <div class="name">
                    <label for="text">News text</label>
                </div>
                <div class="value">
                    <input id="text" name="text">
                </div>
            </div>
            <div class="button-field">
                <input type="submit" value="Enter">
            </div>
            <#if error??>
            <div class="error">
                ${error}
            </div>
            </#if>
        </form>
    </div>
</div>
</@c.page>