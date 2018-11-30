<#import "/spring.ftl" as spring>
<#import "./common.ftlh" as c>

<#macro error field>
    <@spring.bind field/>
    <#list spring.status.errorMessages as error>
        <div class="error">${error}</div>
        <#break/>
    </#list>
</#macro>

<@c.page>
<div class="notice-add form-box">
    <div class="header">Add Notice</div>
    <div class="body">
        <form method="post" action="/addNotice">
            <input type="hidden" name="action" value="addNotice">
            <div class="field">
                <div class="name">
                    <label for="content">Content</label>
                </div>
                <div class="value">
                    <input id="content" name="content" value="${addNoticeForm.content!}"/>
                </div>
                <@error 'addNoticeForm.content'/>
            </div>

            <div class="button-field">
                <input type="submit" value="Add Notice">
            </div>
        </form>
    </div>
</div>
</@c.page>
