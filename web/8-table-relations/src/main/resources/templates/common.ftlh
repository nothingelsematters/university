<#-- @ftlvariable name="notices" type="ru.itmo.wm4.domain.Notice[]" -->
<#-- @ftlvariable name="user" type="ru.itmo.wm4.domain.User" -->
<#import "/spring.ftl" as spring/>
<#macro page>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Codeforces</title>
    <link rel="stylesheet" type="text/css" href="<@spring.url '/css/normalize.css'/>">
    <link rel="stylesheet" type="text/css" href="<@spring.url '/css/style.css'/>">
    <link rel="stylesheet" type="text/css" href="<@spring.url '/css/form.css'/>">
    <link rel="stylesheet" type="text/css" href="<@spring.url '/css/datatable.css'/>">
</head>
<body>
<header>
    <a href="/"><img src="<@spring.url '/img/logo.png'/>" alt="Codeforces" title="Codeforces"/></a>
    <div class="languages">
        <a href="#"><img src="<@spring.url '/img/gb.png'/>" alt="In English" title="In English"/></a>
        <a href="#"><img src="<@spring.url '/img/ru.png'/>" alt="In Russian" title="In Russian"/></a>
    </div>
    <div class="enter-or-register-box">
        <#if user??>
            ${user.login}
            |
            <a href="/logout">Logout</a>
        <#else>
            <a href="/enter">Enter</a>
            |
            <a href="/register">Register</a>
        </#if>
    </div>
    <nav>
        <ul>
            <li><a href="/">Home</a></li>
            <#if user??>
                <li><a href="/users">Users</a></li>
                <li><a href="/notices">Notices</a></li>
                <li><a href="/notice">Add Notice</a></li>
            </#if>
        </ul>
    </nav>
</header>
<div class="middle">
    <aside>
        <#list notices as notice>
            <section>
                <div class="header">
                    ${notice.creationTime}
                </div>
                <div class="body">
                    ${notice.text?html}
                </div>
                <div class="sign">
                    by ${notice.user.login}
                </div>
                <div class="footer">
                    <div class="tags">
                        <#list notice.tags as tag>
                            <ul>
                                ${tag.name}
                            </ul>
                        </#list>
                    </div>
                    <a href="/notice/${notice.id}">View all</a>
                </div>
            </section>
        </#list>
    </aside>
    <main>
        <#nested/>
    </main>
</div>
<footer>
    <a href="#">Codeforces</a> &copy; 2010-2018 by Mike Mirzayanov ${text!}
</footer>
</body>
</html>
</#macro>
