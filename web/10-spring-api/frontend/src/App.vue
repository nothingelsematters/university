<template>
    <!--suppress HtmlUnknownTag -->
    <body id="app">
    <Header :user="user"/>
    <Middle/>
    <Footer/>
    </body>
</template>

<script>
    import axios from 'axios';
    import Header from './components/Header'
    import Middle from './components/Middle'
    import Footer from './components/Footer'

    axios.defaults.baseURL = '/api/1/';

    export default {
        name: 'app',
        data: function () {
            return this.$root.$data;
        },
        props: ['user'],
        components: {
            Header,
            Middle,
            Footer
        },
        beforeCreate() {
            this.$root.$on("onLogout", () => {
                localStorage.removeItem("token");
                this.user = null;
                axios.defaults.headers = {};
            });
            this.$root.$on("onEnter", (token) => {
                localStorage.setItem("token", token);
                axios.defaults.headers = {
                    Authorization: "Bearer " + token
                };
                axios.get("users/authenticated").then(response => {
                    this.user = response.data;
                    this.$root.$emit("onEnterSuccess");
                });
            });
            this.root.$on("onRegistration", () => {
                this.$root.$emit("onRegistrationSuccess");
            });
            this.$root.$on("onAddPost", () => {
                this.$root.$emit("onAddPostSuccess");
            });
        },
        beforeMount() {
            if (localStorage.getItem("token") && !this.user) {
                this.$root.$emit("onEnter", localStorage.getItem("token"));
            }
        }
    }
</script>

<style>
</style>
