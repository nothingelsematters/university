<#-- @ftlvariable name="message" type="java.lang.String" -->

<#import "common.ftl" as c>

<@c.page>
    <#if message??>
        <p>${message}</p>
    </#if>

    <h1>Welcome to the index page</h1>
    <div class="news">
        <#list news as note>
            <div class="note">
                <div class="content">
                    ${note.text}
                </div>
                <div class="sign">
                    by ${note.userName}
                </div>
            </div>
        </#list>
    </div>
</@c.page>
