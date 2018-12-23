<template>
    <form @submit.prevent="onAdd">
        <div>
            <label for="title">
                Title:
            </label>
            <input id="title" class="title" v-model="title"/>
        </div>
        <div>
            <label for="text">
                Text:
            </label>
            <textarea id="text" rows="20" v-model="text"></textarea>
        </div>
        <div class="error">{{error}}</div>
        <div>
            <input type="submit" value="Add"/>
        </div>
    </form>
</template>

<script>
    import axios from 'axios';

    export default {
        data: function () {
            return {
                title: "",
                text: "",
                error: ""
            }
        },
        name: "AddPost",
        beforeMount() {
            this.title = this.text = this.error = "";
            this.$root.$on("onAddPostValidationError", error => this.error = error);
        },
        methods: {
            onAdd: function () {
                this.error = "";
                axios.post("notices", {
                    title: this.title,
                    text: this.text
                }).then(function() {
                    this.$root.$emit("onAddPost");
                }).catch(error => {
                    this.error = error.response.data.message;
                });
            }
        },
        beforeCreate() {
            this.$root.$on("onAddPostSuccess", () => {
                this.$root.$emit("changePage", "Index");
            })
        }
    }
</script>

<style scoped>
    label {
        display: block;
        margin-top: 1rem;
    }
    .title, textarea {
        width: 60%;
        box-sizing: border-box;
    }
    input[type='submit'] {
        margin-top: 1rem;
        width: 6rem;
    }
    .error {
        color: var(--error-color);
    }
</style>
