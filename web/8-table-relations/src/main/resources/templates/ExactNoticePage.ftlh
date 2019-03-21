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
<#if notice??>
    <div class="notice">
        <div class="body">
            ${notice.text?html}
        </div>
        <div class="footer">
            <ul>
                <#list notice.tags as tag>
                    <li>
                        ${tag.name}
                    </li>
                </#list>
            </ul>
            <div class="sign">
                by ${notice.user.login}
            </div>
            <div class="time">
                ${notice.creationTime}
            </div>
        </div>
    </div>
    <div class="comments form-box">
        <#list notice.comments as comment>
            <ul>
                <div class="comment">
                    <div class="user-name">
                        ${comment.user.login}:
                    </div>
                    <div class="body">
                        ${comment.text}
                    </div>
                    <div class="time">
                        ${comment.creationTime}
                    </div>
                </div>
            </ul>
        </#list>
    </div>
    <div class="add-comment form-box">
        <div class="header">New Comment</div>
        <div class="body">
            <form method="post" action="/notice/${notice.id}">
                <div class="field">
                    <div class="name">
                        <label for="text">Text</label>
                    </div>
                    <div class="value">
                        <textarea id="text" rows="10" name="text"></textarea>
                    </div>
                </div>

                <div class="button-field">
                    <input type="submit" value="Add">
                </div>
            </form>
        </div>
    </div>

<#else>
    <div class="message">
        No such notice found
    </div>
</#if>

</@c.page>
