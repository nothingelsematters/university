<!--suppress HtmlUnknownAnchorTarget -->
<template>
    <header>
        <a href="/"><img src="../assets/img/logo.png" alt="Codeforces" title="Codeforces"/></a>
        <div class="languages">
            <a href="#"><img src="../assets/img/gb.png" alt="In English" title="In English"/></a>
            <a href="#"><img src="../assets/img/ru.png" alt="In Russian" title="In Russian"/></a>
        </div>
        <div class="enter-or-register-box">
            <template v-if="user">
                {{user.name}}
                |
                <a href="#page=Logout" @click="logout">Logout</a>
            </template>
            <template v-else>
                <a href="#page=Enter" @click="changePage('Enter')">Enter</a>
                |
                <a href="#page=Enter" @click="changePage('Register')">Register</a>
            </template>
        </div>
        <nav>
            <ul>
                <li><a href="#page=Index" @click="changePage('Index')">Home</a></li>
                <li v-if="user"><a href="#page=AddPost" @click="changePage('AddPost')">Add Post</a></li>
                <li v-if="user"><a href="#page=Users" @click="changePage('Users')">Users</a></li>
            </ul>
        </nav>
    </header>
</template>

<script>
    export default {
        props: ['user'],
        name: "Header",
        beforeCreate() {
            this.$root.$on("onEnterSuccess", () => {
                this.changePage('Index');
            });
            this.$root.$on("onRegistrationSuccess", () => {
                alert("success");
                this.changePage('Enter');
            });
        },
        methods: {
            changePage: function (page) {
                this.$root.$emit("onChangePage", page);
            },
            logout: function() {
                this.$root.$emit("onLogout");
                this.changePage('Index');
            }
        }
    }
</script>

<style scoped>
    header .languages a {
        margin-left: 0.25rem;
    }
</style>
