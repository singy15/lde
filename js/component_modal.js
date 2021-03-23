/*
 * In-window modal dialog component
 */
var modal = {
  name: "modal",
  template: `
    <span v-if="show === true" >
      <div style="position:fixed; top:0px; left:0px; right:0px; bottom:0px; 
          background-color:#000; opacity:0.5;">
      </div>
      
      <div class="bg-dark text-light" :style="modalStyle">
        <slot></slot>
      </div>
    </span>
  `,
  props: [
    "modal-width",
    "modal-height",
    "show"
  ],
  data: function () {
    return {
    };
  },
  computed: {
    modalStyle: function() {
      var self = this;
      return `
        position:fixed; 
        left:calc(50% - ${self.modalWidth / 2}px); 
        top:calc(50% - ${self.modalHeight / 2}px); 
        width:${self.modalWidth}px; 
        height:${self.modalHeight}px;
        border: solid 1px #f8f9fa;
        padding: 10px;
        box-sizing: border-box;
      `;
    }
  },
  methods: {
  },
};

export { modal };