/*
 * Editor component
 */
var editor = {
  template: '<div :id="editorId" style="width: 100%; height: 100%;"></div>',
  props: ["editorId", "content", "lang", "theme"],
  data() {
    return {
      editor: Object,
      beforeContent: "",
      lastPosition: null,
    };
  },
  watch: {
    content(value) {
      this.lastPosition = this.editor.getCursorPosition();

      if (this.beforeContent !== value) {
        this.editor.setValue(value, 1);
        this.editor.moveCursorTo(
          this.lastPosition.row,
          this.lastPosition.column
        );
      }
    },
  },
  mounted() {
    const lang = this.lang || "text";
    const theme = this.theme || "github";

    this.editor = window.ace.edit(this.editorId);
    
    this.editor.setOptions({
      enableBasicAutocompletion: true,
      enableSnippets: true,
      enableLiveAutocompletion: true,
      showGutter: false
    });

    this.editor.$blockScrolling = Infinity;

    this.editor.setValue(this.content, 1);

    this.editor.getSession().setMode(`ace/mode/${lang}`);
    this.editor.setTheme(`ace/theme/${theme}`);

    this.editor.getSession().setTabSize(2);
    this.editor.getSession().setUseSoftTabs(true);

    this.editor.on("change", () => {
      this.beforeContent = this.editor.getValue();
      this.$emit("change-content", this.editor.getValue());
    });

    this.$emit("on-mounted", this.editor);
  },
};

export { editor };