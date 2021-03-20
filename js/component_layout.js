/*
 * Layout component
 */
var layout = {
  template: `
      <div
        :id="layoutId"
        style="left:0px; top:0px; right:0px; bottom:0px;position: absolute;"
      >
      
        <div
          :style="'left:0px; right:0px; top:0px; height:' + north + 'px; border-bottom:solid 1px #CCC; box-sizing: border-box;position: absolute; overflow:hidden;'"
          v-if="!disableNorth"
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
          v-if="!resizableNorth && !disableNorth"
        ></div>

        <div
          :style="'left:0px; right:0px; bottom:0px; height:' + south + 'px; border-top:solid 1px #CCC; box-sizing: border-box;position: absolute; overflow:hidden;'"
          v-if="!disableSouth"
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
          v-if="!resizableSouth && !disableSouth"
        ></div>

        <div
          :style="'left:0px; top:' + north + 'px; bottom:' + south + 'px; width:' + west + 'px; border-right:solid 1px #CCC; box-sizing: border-box;position: absolute; overflow:hidden;'"
          v-if="!disableWest"
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
          v-if="!resizableWest && !disableWest"
        ></div>

        <div
          :style="'right:0px; top:' + north + 'px; bottom:' + south + 'px; width:' + east + 'px; border-left:solid 1px #CCC; box-sizing: border-box;position: absolute; overflow:hidden;'"
          v-if="!disableEast"
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
          v-if="!resizableEast && !disableEast"
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
    "disable-north",
    "disable-south",
    "disable-west",
    "disable-east",
  ],
  data() {
    return {
      south: (this.disableSouth)? 0 : (parseInt(this.sizeSouth, 10) ? parseInt(this.sizeSouth, 10) : 100),
      north: (this.disableNorth)? 0 : (parseInt(this.sizeNorth, 10) ? parseInt(this.sizeNorth, 10) : 100),
      west: (this.disableWest)? 0 : (parseInt(this.sizeWest, 10) ? parseInt(this.sizeWest, 10) : 100),
      east: (this.disableEast)? 0 : (parseInt(this.sizeEast, 10) ? parseInt(this.sizeEast, 10) : 100),
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
        this.$emit("on-resize-west", this.west);
      }

      if (handle === "east") {
        if (this.resizableEast === "false") {
          event.stopPropagation();
          event.preventDefault();
          return false;
        }
        this.east = this.oldValue + (this.originX - event.screenX);
        this.$emit("on-resize-east", this.east);
      }

      if (handle === "north") {
        if (this.resizableNorth === "false") {
          event.stopPropagation();
          event.preventDefault();
          return false;
        }
        this.north = this.oldValue + (event.screenY - this.originY);
        this.$emit("on-resize-north", this.north);
      }

      if (handle === "south") {
        if (this.resizableSouth === "false") {
          event.stopPropagation();
          event.preventDefault();
          return false;
        }
        this.south = this.oldValue + (this.originY - event.screenY);
        this.$emit("on-resize-south", this.south);
      }
    },
  },
  mounted() {},
};

export { layout };