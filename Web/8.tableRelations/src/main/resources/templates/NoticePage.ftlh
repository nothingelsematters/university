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
<div class="form-box">
    <div class="header">New Notice</div>
    <div class="body">
        <form method="post" action="/notice">
            <div class="field">
                <div class="name">
                    <label for="text">Text</label>
                </div>
                <div class="value">
                    <textarea id="text" rows="10" name="text">${noticeForm.text!?html}</textarea>
                </div>
                <@error 'noticeForm.text'/>
            </div>
            <div class="field">
                 <div class="name">
                    <label for="tags">Tags</label>
                 </div>
                <div class="value">
                     <input id="tags" name="tags" value="${noticeForm.tags!}"></input>
                </div>
                <@error 'noticeForm.tags'/>
            </div>
            <div class="button-field">
                <input type="submit" value="Add">
            </div>
        </form>
    </div>
</div>
</@c.page>
