<#import "common.ftl" as c>

<@c.page>
<#list talks as talk>
    <ul>
        <div class="name">${talk.source} -> </div>
        <div class="target">${talk.target}</div>
        <div class="text">${talk.text}</div>
        <time>${talk.time}<time>
    </ul>
</#list>

<div class="form-box">
    <div class="header">Send Message</div>
    <div class="body">
        <form method="post" action="/talks">
            <input type="hidden" name="action" value="talks">
            <div class="field">
                <div class="name">
                    <label for="target">Send To/Email</label>
                </div>
                <div class="value">
                    <input id="target" name="target" value="${target!}"/>
                </div>
            </div>
            <div class="field">
                <div class="name">
                    <label for="text">Text</label>
                </div>
                <div class="value">
                    <input id="text" name="text" value="${text!}"/>
                </div>
            </div>
            <div class="button-field">
                <input type="submit" value="Send">
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
