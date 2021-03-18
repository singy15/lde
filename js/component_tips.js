/*
 * Tips component
 */
var tips = {
  template: `
      <span @mouseover="mouseOver" @mouseleave="mouseLeave">
        <slot>
        </slot>
      </span>
  `,
  props: {
    msg: String,
  },
  data: function () {
    return {
      show: false,
    };
  },
  methods: {
    showTips: function () {
      console.log("showTips");
    },
    mouseOver: function () {
      this.show = true;
      this.$emit("on-hover", this.msg);
    },
    mouseLeave: function () {
      this.show = false;
      this.$emit("on-hover", "");
    },
  },
};

export { tips };