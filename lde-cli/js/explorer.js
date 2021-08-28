import Vue from "vue";

// layout component
Vue.component("layout", {
  template: `
      <div
        :id="layoutId"
        style="left:0px; top:0px; right:0px; bottom:0px;position: absolute;"
      >
      
        <div
          :style="'left:0px; right:0px; top:0px; height:' + north + 'px; border-bottom:solid 1px #CCC; box-sizing: border-box;position: absolute; overflow:hidden;'"
        >
          <div :style="'width:100%;height:100%;'+styleNorth" :class="classNorth">
            <slot name="north"></slot>
          </div>
        </div>
        <div
          :style="'left:0px; right:0px; top:' + (north - borderWidth) + 'px; height:' + borderWidth + 'px; position: absolute;z-index: 1000;cursor:pointer;'"
          draggable="true"
          @dragstart="dragstart('north', $event)"
          @drag="drag('north', $event)"
        ></div>

        <div
          :style="'left:0px; right:0px; bottom:0px; height:' + south + 'px; border-top:solid 1px #CCC; box-sizing: border-box;position: absolute; overflow:hidden;'"
        >
          <div :style="'width:100%;height:100%;'+styleSouth" :class="classSouth">
            <slot name="south"></slot>
          </div>
        </div>
        <div
          :style="'left:0px; right:0px; bottom:' + (south - borderWidth) + 'px; height:' + borderWidth + 'px;position: absolute;z-index: 1000;cursor:pointer;'"
          draggable="true"
          @dragstart="dragstart('south', $event)"
          @drag="drag('south', $event)"
        ></div>

        <div
          :style="'left:0px; top:' + north + 'px; bottom:' + south + 'px; width:' + west + 'px; border-right:solid 1px #CCC; box-sizing: border-box;position: absolute; overflow:hidden;'"
        >
          <div :style="'width:100%;height:100%;'+styleWest" :class="classWest">
            <slot name="west"></slot>
          </div>
        </div>
        <div
          :style="'left:' + (west - borderWidth) + 'px; top:' + north + 'px; bottom:' + south + 'px; width:' + borderWidth + 'px;position: absolute;z-index: 1000;cursor:pointer;'"
          draggable="true"
          @dragstart="dragstart('west', $event)"
          @drag="drag('west', $event)"
        ></div>

        <div
          :style="'right:0px; top:' + north + 'px; bottom:' + south + 'px; width:' + east + 'px; border-left:solid 1px #CCC; box-sizing: border-box;position: absolute; overflow:hidden;'"
        >
          <div :style="'width:100%;height:100%;'+styleEast" :class="classEast">
            <slot name="east"></slot>
          </div>
        </div>
        <div
          :style="'right:' + (east - borderWidth ) + 'px; top:' + north + 'px; bottom:' + south + 'px; width:' + borderWidth + 'px;position: absolute;z-index: 1000;cursor:pointer;'"
          draggable="true"
          @dragstart="dragstart('east', $event)"
          @drag="drag('east', $event)"
        ></div>

        <div
          :style="'left:' + west + 'px; right:' + east + 'px; top:' + north + 'px; bottom:' + south + 'px;box-sizing: border-box;position: absolute; overflow:hidden;'"
        >
          <div :style="'width:100%;height:100%;'+styleCenter" :class="classCenter">
            <slot name="center"></slot>
          </div>
        </div>
      </div>
  `,
  props: [
    "layout-id",
    "border-width",
    "size-north",
    "size-south",
    "size-east",
    "size-west",
    "style-north",
    "style-south",
    "style-west",
    "style-east",
    "style-center",
    "class-north",
    "class-south",
    "class-west",
    "class-east",
    "class-center",
    "resizable-north",
    "resizable-south",
    "resizable-west",
    "resizable-east",
  ],
  data() {
    return {
      south: parseInt(this.sizeSouth, 10) ? parseInt(this.sizeSouth, 10) : 100,
      north: parseInt(this.sizeNorth, 10) ? parseInt(this.sizeNorth, 10) : 100,
      west: parseInt(this.sizeWest, 10) ? parseInt(this.sizeWest, 10) : 100,
      east: parseInt(this.sizeEast, 10) ? parseInt(this.sizeEast, 10) : 100,
      originX: null,
      currentX: null,
      originY: null,
      currentY: null,
      oldValue: null,
    };
  },
  methods: {
    dragstart: function (handle, event) {
      event.dataTransfer.setDragImage(new Image(), 0, 0);

      if (event.clientX === 0 || event.clientY === 0) {
        return;
      }

      this.originX = event.screenX;
      this.originY = event.screenY;

      var parent = document.getElementById(this.layoutId);

      if (handle === "west") {
        if (this.resizableWest === "false") {
          event.stopPropagation();
          event.preventDefault();
          return false;
        }
        this.oldValue = this.west;
      }

      if (handle === "east") {
        if (this.resizableEast === "false") {
          event.stopPropagation();
          event.preventDefault();
          return false;
        }
        this.oldValue = this.east;
      }

      if (handle === "north") {
        if (this.resizableNorth === "false") {
          event.stopPropagation();
          event.preventDefault();
          return false;
        }
        this.oldValue = this.north;
      }

      if (handle === "south") {
        if (this.resizableSouth === "false") {
          event.stopPropagation();
          event.preventDefault();
          return false;
        }
        this.oldValue = this.south;
      }

      console.log("init", this.oldValue);
    },
    drag: function (handle, event) {
      if (event.clientX === 0 || event.clientY === 0) {
        return;
      }

      var parent = document.getElementById(this.layoutId);

      if (handle === "west") {
        if (this.resizableWest === "false") {
          event.stopPropagation();
          event.preventDefault();
          return false;
        }
        this.west = this.oldValue + (event.screenX - this.originX);
      }

      if (handle === "east") {
        if (this.resizableEast === "false") {
          event.stopPropagation();
          event.preventDefault();
          return false;
        }
        this.east = this.oldValue + (this.originX - event.screenX);
      }

      if (handle === "north") {
        if (this.resizableNorth === "false") {
          event.stopPropagation();
          event.preventDefault();
          return false;
        }
        this.north = this.oldValue + (event.screenY - this.originY);
      }

      if (handle === "south") {
        if (this.resizableSouth === "false") {
          event.stopPropagation();
          event.preventDefault();
          return false;
        }
        this.south = this.oldValue + (this.originY - event.screenY);
      }
    },
  },
  mounted() {},
});

// Editor component
Vue.component("Editor", {
  template: '<div :id="editorId" style="width: 100%; height: 100%;"></div>',
  props: ["editorId", "content", "lang", "theme"],
  data() {
    return {
      editor: Object,
      beforeContent: "",
      lastPosition: null,
      filepath: "",
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
    lang(value) {
      this.editor.getSession().setMode(`ace/mode/${value}`);
    },
  },
  mounted() {
    const lang = this.lang || "text";
    const theme = this.theme || "github";

    this.editor = window.ace.edit(this.editorId);
    this.editor.setValue(this.content, 1);

    this.editor.getSession().setMode(`ace/mode/${lang}`);
    this.editor.setTheme(`ace/theme/${theme}`);

    this.editor.getSession().setTabSize(2);
    this.editor.getSession().setUseSoftTabs(true);

    this.editor.on("change", () => {
      this.beforeContent = this.editor.getValue();
      this.$emit("change-content", this.editor.getValue());
    });
  },
});

// Register tree-item component
Vue.component("tree-item", {
  template: `
      <span style="user-select: none;">
        <span
          :class="{bold: isFolder}"
          style="white-space:nowrap;"
          @dblclick="dblClick">
          
          <span :style="'margin-left: ' + ((level * 10)).toString() + 'px;'"></span>
          <span :class="icon" @click="toggle"></span>
          <span 
            :style="(item.name === selectedItem)? 'text-decoration: underline; cursor: pointer; white-space: nowrap;' : 'cursor: pointer; white-space: nowrap;'" 
            @click="clicked">
            &nbsp;{{ item.name }}
          </span>

        </span>
        <br>
        <span v-show="isOpen" v-if="isFolder">
          <tree-item
            class="item"
            v-for="(child, index) in sorted"
            :key="index"
            :item="child"
            :level="level + 1"
            @dbl-click="$emit('dbl-click', $event)"
            @add-item="$emit('add-item', $event)"
            @selected="$emit('selected', $event)"
            :selected-item="selectedItem"
          ></tree-item>
        </span>
      </span>
  `,
  props: {
    item: Object,
    level: Number,
    open: Boolean,
    selectedItem: String,
  },
  data: function () {
    return {
      isOpen: this.open || false,
      selected: null,
    };
  },
  computed: {
    sorted: function () {
      return _.sortBy(this.item.children, function (c) {
        return c.entry + "-" + c.name;
      });
    },
    isFolder: function () {
      return this.item.children && this.item.children.length;
    },
    icon: function () {
      /*
      if (this.item.name === "ROOT") {
        if (this.isOpen) {
          return "icon-folder-open";
        } else {
          return "icon-folder";
        }
      } else {
        switch (this.item.info.moduleType) {
          case "view":
            return "icon-display";
          case "model":
            return "icon-database";
          default:
            return "icon-file-empty";
        }
      }
      */

      if (this.item.entry === "directory") {
        return this.isOpen ? "icon-folder-open" : "icon-folder";
      } else {
        return "icon-file-empty";
      }
    },
  },
  methods: {
    toggle: function () {
      if (this.isFolder) {
        this.isOpen = !this.isOpen;
      }
    },
    dblClick: function () {
      // if (!this.isFolder) {
      this.$emit("dbl-click", this.item);
      // this.isOpen = true;
      // }
    },
    clicked: function () {
      console.log("selectedItem", this.selectedItem);
      this.$emit("selected", this.item);
    },
  },
});

var app = new Vue({
  el: "#app",
  delimiters: ["[[", "]]"],
  data: {
  },
  methods: {
  },
});

