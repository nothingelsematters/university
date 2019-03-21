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
<div class="enter form-box">
    <div class="header">Enter</div>
    <div class="body">
        <form method="post" action="/enter">
            <div class="field">
                <div class="name">
                    <label for="login">Login</label>
                </div>
                <div class="value">
                    <input id="login" name="login" value="${enterForm.login!}"/>
                </div>
                <@error 'enterForm.login'/>
            </div>

            <div class="field">
                <div class="name">
                    <label for="password">Password</label>
                </div>
                <div class="value">
                    <input id="password" type="password" name="password" value="${password!}"/>
                </div>
                <@error 'enterForm.password'/>
            </div>

            <div class="button-field">
                <input type="submit" value="Enter">
            </div>
        </form>
    </div>
</div>
</@c.page>
