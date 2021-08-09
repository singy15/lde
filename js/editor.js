import Vue from "vue";
import { layout } from "./component_layout.js";

var storageRootKey = "lde/editor/";

/*
 * Save to storage
 */
function setStorage(key, obj) {
  localStorage.setItem(storageRootKey + key, JSON.stringify(obj));
}

/*
 * Get from storage
 */
function getStorage(key, dflt) {
  var item = localStorage.getItem(storageRootKey + key);
  return item ? JSON.parse(item) : dflt;
}

// Editor component
Vue.component("Editor", {
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
    lang(value) {
      this.editor.getSession().setMode(`ace/mode/${value}`);
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
      showGutter: true
    });
    
    // this.editor.setKeyboardHandler("ace/keyboard/vim");

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
  components: {
    "layout": layout
  },
  data: {
    featureId: featureId,
    src: "",
    filepath: target,
    language: "text",
    treeData: { name: "ROOT", id: "ROOT", children: [] },
    selectedItem: null,
    blocked: true,
    editor: null,
    evalOnSave: getStorage("evalOnSave", false)
  },
  watch: {
    evalOnSave: function(val) {
      setStorage("evalOnSave", val);
    }
  },
  methods: {
    onMountedSrcEditor: function(editor) {
      this.editor = editor;
    },
    changeContentSrc: function (val) {
      if (app.src !== val) {
        app.src = val;
      }
    },
    getFile: function () {
      this.blocked = true;
      var self = this;
      // fetch(encodeURI("/editor/file?filepath=" + this.filepath), {
      fetch(encodeURI("/editor/file?filepath=" + target), {
        method: "GET",
        headers: {
          "Content-Type": "application/json;charset=utf-8",
        },
        cache: "no-store",
      })
        .then(function (response) {
          return response.json();
        })
        .then(function (data) {
          console.log(data);
          self.src = data.data.content;

          switch (data.data.extension) {
            case "js":
              self.language = "javascript";
              break;

            case "lisp":
              self.language = "lisp";
              break;

            case "ejs":
              self.language = "html";
              break;

            case "html":
              self.language = "html";
              break;
          }
          
          self.blocked = false;
        });
    },
    postFile: function (multithread) {
      this.blocked = true;
      var self = this;
      console.log(self.evalOnSave);
      fetch(encodeURI("/editor/file?filepath=" + this.filepath), {
        method: "POST",
        body: JSON.stringify({
          content: self.src,
          evalOnSave: self.evalOnSave,
          multithread: multithread
        }),
        headers: {
          "Content-Type": "application/json;charset=utf-8",
        },
        cache: "no-store",
      })
        .then(function (response) {
          return response.json();
        })
        .then(function (data) {
          console.log(data);
          self.getFile();
        });
    },
    refreshFilelist: function () {
      var self = this;
      fetch(encodeURI("/editor/filelist?basepath=client"), {
        method: "GET",
        headers: {
          "Content-Type": "application/json;charset=utf-8",
        },
        cache: "no-store",
      })
        .then(function (response) {
          return response.json();
        })
        .then(function (data) {
          console.log(data);

          // Extract paths
          var files = data.data;

          // Split paths
          var paths = _.map(files, function (f) {
            return f.split("/");
          });
          console.log(paths);

          // Create root structure
          var strct = {
            name: "ROOT",
            id: "ROOT",
            entry: "directory",
            path: "ROOT",
            children: [],
          };

          // Iterate over all aplitted paths
          _.each(paths, function (path) {
            console.log(path);

            // Set current path to root
            var cur = strct;

            for (var i = 0; i < path.length; i++) {
              var p = path[i];

              if (i == path.length - 1) {
                // Add file entry
                var file = {
                  name: p,
                  id: p,
                  entry: "file",
                  path: path.join("/"),
                  children: [],
                  info: { moduleType: "js" },
                };
                cur.children.push(file);
              } else {
                // Add directory entry

                // Filter current children to check directory already exists to structure
                var c = _.filter(cur.children, function (cf) {
                  return cf.name === p;
                });

                if (c.length > 0) {
                  console.log("path hit");
                  cur = c[0];
                  console.log("current", cur);
                } else {
                  console.log("path not hit");
                  var next = {
                    name: p,
                    id: p,
                    entry: "directory",
                    path: path.join("/"),
                    children: [],
                    info: { moduleType: "js" },
                  };
                  cur.children.push(next);
                  cur = next;
                  console.log("current", cur);
                }

                console.log(strct);
              }
            }
          });
          console.log(strct);

          self.treeData = {
            name: "ROOT",
            id: "ROOT",
            children: _.map(data.data, function (f) {
              return {
                name: f,
                id: f,
                info: { moduleType: "js" },
                entry: "file",
                fileType: "js",
                path: f,
                children: null,
              };
            }),
            /*[
              {
                name: "easytask-index",
                path:
                  "C:/Users/kedama/quicklisp/local-projects/xra/client/easytask-index.xmd",
                info: { moduleType: "view" },
                entry: "module",
                children: [
                  {
                    name: "js",
                    id: "easytask-index/js",
                    info: { moduleType: "js" },
                    entry: "file",
                    fileType: "js",
                    path: "js/easytask-index.js",
                    children: null,
                  },
                ],
              },
            ],*/
          };

          self.treeData = strct;
        });
    },
    selectItem: function (item) {
      this.selectedItem = item.id;
    },
    dblClickItem: function (item) {
      this.selectedItem = item.id;
      this.filepath = item.path;
      this.getFile();
    },
    evaluate: function(multithread) {
      var self = this;
      console.log(this.editor.getSelectedText())
      
      fetch(encodeURI("/editor/eval"), {
        method: "POST",
        body: JSON.stringify({
          src: self.editor.getSelectedText(),
          multithread: multithread
        }),
        headers: {
          "Content-Type": "application/json;charset=utf-8",
        },
        cache: "no-store",
      })
        .then(function (response) {
          return response.json();
        })
        .then(function (data) {
          console.log(data);
          
        });
    }
  },
});

shortcut.add("Ctrl+Shift+S", function () {
  app.postFile(true);
});

shortcut.add("Ctrl+S", function () {
  app.postFile(false);
});

shortcut.add("Ctrl+Shift+B", function () {
  app.evaluate(true);
});

shortcut.add("Ctrl+B", function () {
  app.evaluate(false);
});


app.refreshFilelist();
app.getFile();

console.log("editor");
