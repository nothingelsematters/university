<template>
    <div class="enter form-box">
        <div class="header">Enter</div>
        <div class="body">
            <form @submit.prevent="onEnter">
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
                        <input id="password" type="password" name="password"/>
                    </div>
                </div>

                <div class="error">{{error}}</div>

                <div class="button-field">
                    <input type="submit" value="Enter">
                </div>
            </form>
        </div>
    </div>
</template>

<script>
    export default {
        data: function() {
            return {
                login: "",
                password: "",
                error: ""
            }
        },
        name: "Enter",
        beforeCreate() {
            this.$root.$on("onEnterValidationError", (error) => {
                this.error = error;
            });
        },
        beforeMount() {
            this.login = "";
            this.password = "";
            this.error = "";
        }, methods: {
            onEnter: function () {
                this.$root.$emit("onEnter", this.login, this.password);
            }
        }
    }
</script>

<style scoped>

</style>
