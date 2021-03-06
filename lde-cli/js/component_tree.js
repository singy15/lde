import _ from 'underscore';

/*
 * Tree component
 */
var tree = {
  name: "tree",
  template: `
      <span style="white-space:nowrap;  user-select: none;">
        <span v-if="item.base !== ''">
      
          <!-- Margin -->
          <span :style="'margin-left: ' + (((level - 1) * 10)).toString() + 'px;'"></span>
          
          <!-- Opener -->
          <span style="display:inline-block; width:10px;" @click="toggle">
            <!-- <img v-if="isDirectory && (!isOpen)" src="/public/svg/plus.svg" width="10" height="10"/> -->
            <span v-if="isDirectory && (!isOpen)" uk-icon="icon: chevron-right" width="10" height="10"></span>
            <!-- <img v-if="isDirectory && (isOpen)" src="/public/svg/minus.svg" width="10" height="10"/> -->
            <span v-if="isDirectory && (isOpen)" uk-icon="icon: chevron-down" width="10" height="10"></span>
          </span>
          
          <!-- Icon -->
          <span v-if="isBaseEntry" uk-icon="icon: thumbnails" width="16" height="16"></span>
          <!-- <img v-if="isBaseEntry" src="/public/svg/circle.svg" width="12" height="10"/> -->
          <span v-if="!isBaseEntry && isDirectory" uk-icon="icon: folder" width="16" height="16"></span>
          <!-- <img v-if="!isBaseEntry && isDirectory" src="/public/svg/directory.svg" width="12" height="10"/> -->
          <span v-if="!isBaseEntry && !isDirectory" uk-icon="icon: file" width="14" height="14"></span>
          <!-- <img v-if="!isBaseEntry && !isDirectory" src="/public/svg/file.svg" width="12" height="12"/> -->
          
          <!-- Display name -->
          <span :style="(item === selectedItem)? 'text-decoration: underline; cursor: pointer; white-space: nowrap;' : 'cursor: pointer; white-space: nowrap;'" 
              @click="clicked"
              @dblclick="dblClick">
            {{ dispName }}
          </span>
          
          <br>
        
        </span>
        
        <!-- Children -->
        <span v-show="isOpen">
          <tree
            v-for="(child,index) in listed"
            :item="child"
            :key="index"
            :level="level+1"
            :parent="item.path"
            @dbl-click="$emit('dbl-click', $event)"
            @add-item="$emit('add-item', $event)"
            @selected="$emit('selected', $event)"
            @on-open="$emit('on-open', $event)"
            :selected-item="selectedItem"
          ></tree>
        </span>
      </span>

  `,
  props: {
    item: Object,
    level: Number,
    parent: String,
    open: Boolean,
    selectedItem: Object,
  },
  data: function () {
    return {
      isOpen: this.open || false,
      selected: null,
    };
  },
  computed: {
    listed: function () {
      var self = this;
      var ls = [];
      for(var key in self.item.children) {
        ls.push(self.item.children[key]);
      }
      
      return _.sortBy(ls, function (c) {
        return ((c.path.match(/^.*\/$/))? "0" : "1") + c.path;
      });
    },
    dispName: function() {
      // DEBUG:
      // return this.item.path;
      
      if(this.item.base === this.item.path) {
        return this.splittedPath[this.splittedPath.length - 2];
      }
      
      var name = this.item.path.replace(this.parent, "");
      if(name === "") {
        return "ENTRIES";
      }
      return name;
    },
    splittedPath: function() {
      return this.item.path.split("/");
    },
    isDirectory: function() {
      return this.splittedPath[this.splittedPath.length - 1] === "";
    },
    isBaseEntry: function() {
      return this.item.base === this.item.path;
    }
  },
  methods: {
    toggle: function () {
      if (this.isDirectory) {
        this.isOpen = !this.isOpen;
        if(this.isOpen) {
          this.item.children = [];
          this.$emit("on-open", this.item);
        }
      }
    },
    dblClick: function () {
      // if (!this.isFolder) {
      console.log("dbl-click");
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

export { tree };
