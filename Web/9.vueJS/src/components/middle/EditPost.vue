<template>
    <form @submit.prevent="onEdit">
        <div>
            <label for="id">
                ID:
            </label>
            <input id="id" class="id" v-model="id"/>
        </div>
        <div>
            <label for="text">
                Text:
            </label>
            <textarea id="text" rows="20" v-model="text"></textarea>
        </div>
        <div class="error">{{error}}</div>
        <div>
            <input type="submit" value="Edit"/>
        </div>
    </form>
</template>

<script>
    export default {
        data: function () {
            return {
                id: "",
                text: "",
                error: ""
            }
        },
        name: "EditPost",
        beforeMount() {
            this.id = this.text = this.error = "";
            this.$root.$on("onEditPostValidationError", error => this.error = error);
        }, methods: {
            onEdit: function () {
                this.$root.$emit("onEditPost", this.id, this.text);
            }
        }
    }
</script>

<style scoped>
    label {
        display: block;
        margin-top: 1rem;
    }
    .id, textarea {
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
