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
<div class="register form-box">
    <div class="header">Register</div>
    <div class="body">
        <form method="post" action="/register">
            <input type="hidden" name="action" value="register">
            <div class="field">
                <div class="name">
                    <label for="login">Login</label>
                </div>
                <div class="value">
                    <input id="login" name="login" value="${registerForm.login!}"/>
                </div>
                <@error 'registerForm.login'/>
            </div>

            <div class="field">
                <div class="name">
                    <label for="password">Password</label>
                </div>
                <div class="value">
                    <input id="password" type="password" name="password" value="${password!}"/>
                </div>
                <@error 'registerForm.password'/>
            </div>

            <div class="button-field">
                <input type="submit" value="Register">
            </div>
        </form>
    </div>
</div>
</@c.page>
