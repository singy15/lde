/*
 * Tree-item component
 */
var treeitem = {
  name: "treeitem",
  template: `
      <span style="user-select: none;">
        <span
          :class="{bold: isFolder}"
          style="white-space:nowrap;"
          @dblclick="dblClick">
          
          <span :style="'margin-left: ' + ((level * 10)).toString() + 'px;'"></span>
          <span style="display:inline-block; width:10px;" :class="icon" @click="toggle">{{ (item.entry === "directory")? ((isOpen)? "-" : "+") : "" }}</span>
          <span 
            :style="(item.name === selectedItem)? 'text-decoration: underline; cursor: pointer; white-space: nowrap;' : 'cursor: pointer; white-space: nowrap;'" 
            @click="clicked">
            &nbsp;{{ item.name }}{{ (item.entry === "directory")? "/" : "" }}
          </span>

        </span>
        <br>
        <span v-show="isOpen" v-if="isFolder">
          <treeitem
            class="item"
            v-for="(child, index) in sorted"
            :key="index"
            :item="child"
            :level="level + 1"
            @dbl-click="$emit('dbl-click', $event)"
            @add-item="$emit('add-item', $event)"
            @selected="$emit('selected', $event)"
            @on-open="$emit('on-open', $event)"
            :selected-item="selectedItem"
          ></treeitem>
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

      // if (this.item.entry === "directory") {
      //   return this.isOpen ? "icon-folder-open" : "icon-folder";
      // } else {
      //   return "icon-file-empty";
      // }
      return "";
    },
  },
  methods: {
    toggle: function () {
      if (this.item.entry === "directory") {
        this.isOpen = !this.isOpen;
        if(this.isOpen) {
          this.item.children = [];
          this.$emit("on-open", this.item);
        }
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
};

export { treeitem };