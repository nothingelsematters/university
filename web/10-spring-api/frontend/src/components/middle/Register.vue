<template>
    <div class="register form-box">
        <div class="header">Registration</div>
        <div class="body">
            <form @submit.prevent="onRegistration">
                <div class="field">
                    <div class="name">
                        <label for="name">Name</label>
                    </div>
                    <div class="value">
                        <input id="name" name="name" v-model="name"/>
                    </div>
                </div>

                <div class="field">
                    <div class="name">
                        <label for="login">Login</label>
                    </div>
                    <div class="value">
                        <input id="login" name="login" v-model="login"/>
                    </div>
                </div>

                <div class="field">
                    <div class="name">
                        <label for="password">Password</label>
                    </div>
                    <div class="value">
                        <input id="password" type="password" name="password" v-model="password"/>
                    </div>
                </div>

                <div class="error">{{error}}</div>

                <div class="button-field">
                    <input type="submit" value="Register">
                </div>
            </form>
        </div>
    </div>
</template>

<script>
    import axios from 'axios';

    axios.defaults.baseURL = '/api/1/';

    export default {
        name: "Register",
        data: function() {
            return {
                name: "",
                login: "",
                password: "",
                error: ""
            }
        },
        beforeMount() {
            this.name = this.login = this.password = this.error = "";
            this.$root.$on("onRegistrationValidationError", error => this.error = error);
        },
        methods: {
            onRegistration: function() {
                this.error = "";
                axios.post("users", {
                    name: this.name,
                    login: this.login,
                    password: this.password
                }).then(function() {
                    this.$root.$emit("onRegistration");
                }).catch(error => {
                    this.error = error.response.data.message;
                });
            }
        }
    }
</script>

<style scoped>

</style>
