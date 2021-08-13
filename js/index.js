import Vue from "vue";
import { layout } from "./component_layout.js";
import { editor } from "./component_editor.js";
import { treeitem } from "./component_treeitem.js";
import { tips } from "./component_tips.js";
import { StorageUtil } from "./storage_util.js";
import { modal } from "./component_modal.js";

var storageUtil = new StorageUtil("lde/index/");

/*
 * Vue instance
 */
window.app = new Vue({
  el: "#app",
  delimiters: ["[[", "]]"],
  components: {
    "layout": layout
    ,"editor": editor
    ,"treeitem": treeitem
    ,"tips": tips
    ,"modal": modal
  },
  data: {
    fileType: "",
    fileName: "",
    files: [
      {
        moduleId: "module1",
        fileName: "package-module1.lisp",
        fileType: "package",
      },
      { moduleId: "module1", fileName: "module1.lisp", fileType: "implement" },
      { moduleId: "module1", fileName: "module1.js", fileType: "js" },
      { moduleId: "module1", fileName: "module1.ejs", fileType: "ejs" },
    ],
    // treeData: modules,
    treeData: {
            name: "ROOT",
            id: "ROOT",
            entry: "directory",
            path: "ROOT",
            children: [],
          },
    editorText: "",
    selectedModule: null,
    fileSelected: false,
    iframeSrc: "",
    some: "",
    selectedTab: "tab2",
    tabs: [],
    statusMessage: "",
    curPath: curPath,
    sessionOpened: false,
    socket: null,
    contentConsole: "",
    contentConsoleInput: storageUtil.getStorage("contentConsoleInput", ""),
    editorConsole: null,
    editorConsoleInput: null,
    nameNewFile: "",
    selectedItem: null,
    southSize: storageUtil.getStorage("southSize", 200),
    southEastSize: storageUtil.getStorage("southEastSize", 400),
    showFileCreateModal: false,
    showFileDeleteModal: false
  },
  methods: {
    onMountedEditorConsole: function (editor) {
      this.editorConsole = editor;
    },
    changeContentConsole: function (val) {
      if (app.contentConsole !== val) {
        app.contentConsole = val;

        var row = this.editorConsole.session.getLength() - 1;
        this.editorConsole.gotoLine(row + 1, 0);
      }
    },
    onMountedEditorConsoleInput: function (editor) {
      this.editorConsoleInput = editor;
    },
    changeContentConsoleInput: function (val) {
      if (app.contentConsoleInput !== val) {
        app.contentConsoleInput = val;
        storageUtil.setStorage("contentConsoleInput", val);
      }
    },
    changeRoot: function() {
      this.curPath = this.curPath.replace(/\\/g, '/');
      if(this.curPath[this.curPath.length -1] !== '/') {
        this.curPath = this.curPath + "/";
      }
      console.log(this.curPath);
      var self = this;
      fetch(encodeURI("/set-root"), {
        method: "POST",
        headers: {
          "Content-Type": "application/json;charset=utf-8",
        },
        body: JSON.stringify({
          path: this.curPath 
        }),
        cache: "no-store",
      })
        .then(function (response) {
          return response.json();
        })
        .then(function (data) {
          location.href = "/";
        });
    },
    // Refresh file list
    refreshFilelist: function (path) {
      var self = this;
      fetch(encodeURI("/lis"), {
        method: "POST",
        headers: {
          "Content-Type": "application/json;charset=utf-8",
        },
        body: JSON.stringify({
          path: path 
        }),
        cache: "no-store",
      })
        .then(function (response) {
          return response.json();
        })
        .then(function (data) {
          // Create root structure
          var splitted = self.curPath.split("/");
          if(path === "") {
            self.treeData = {
              name: splitted[splitted.length - 2],
              id: "ROOT",
              entry: "directory",
              path: "",
              children: [],
            };
          }

          // Iterate over all aplitted paths
          _.each(data.data, function (f) {
            self.addNodeToStructure(f);
          });
        });
    },
    addNodeToStructure: function(f) {
      var self = this;
      
      var path = f.split("/");

      // Set current path to root
      var cur = self.treeData;

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
          
          if(file.name !== "") { 
            cur.children.push(file);
          }
        } else {
          // Add directory entry

          // Filter current children to check directory already exists to structure
          var c = _.filter(cur.children, function (cf) {
            return cf.name === p;
          });

          if (c.length > 0) {
            cur = c[0];
          } else {
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
          }
        }
      }
    },
    changeContentEditor: function (val) {
      if (app.editorText !== val) {
        app.editorText = val;
      }
    },
    dblClick: function (item) {
      var tabName = "TAB:" + app.tabs.length.toString();

      // for (var i = 0; i < this.tabs.length; i++) {
      //   if (this.tabs[i].src === `/editor?feature-id=${item.name}`) {
      //     this.selectedTab = this.tabs[i].name;
      //     return;
      //   }
      // }

      console.log(item, {
        name: tabName,
        src: `/editor?target=${item.path}`,
        dispName: item.name, 
      });
      app.tabs.push({
        name: tabName,
        src: `/editor?target=${item.path}`,
        dispName: item.name, 
      });

      app.selectedTab = tabName;
    },
    addItem: function (item) {
      item.children.push({
        name: "new stuff",
      });
    },
    moduleSelected: function (item) {
      console.log("select", item);
      app.selectedModule = item.name;
      this.selectedItem = item;
    },
    someInput: function (value) {
      console.log(value);
    },
    createModule: function () {
      var tabName = "TAB:" + app.tabs.length.toString();
      app.tabs.push({
        name: tabName,
        dispName: "xra-core-module",
        src: "/xra-core-module",
      });
      app.selectedTab = tabName;
    },
    refreshModule: function () {
      fetch("/xra-core-index/modules", {
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
          app.treeData = data;
        });
    },
    onOpen: function(item) {
      this.refreshFilelist(item.path);
    },
    selectTab: function (tab) {
      app.selectedTab = tab.name;
    },
    closeTab: function (tab) {
      var index = this.tabs.indexOf(tab);
      this.tabs = this.tabs.filter((x) => x.name != tab.name);
      if (this.tabs[index]) {
        this.selectedTab = this.tabs[index].name;
      } else if (this.tabs.length >= 1) {
        app.selectedTab = this.tabs[this.tabs.length - 1].name;
      }
    },
    deleteModule: function () {
      console.log("delete module", app.selectedModule);
      fetch("/xra-core-index/modules?module-id=" + app.selectedModule, {
        method: "DELETE",
        headers: {
          "Content-Type": "application/json;charset=utf-8",
        },
        body: "",
        cache: "no-store",
      })
        .then(function (response) {
          return response.json();
        })
        .then(function (data) {});
    },
    msgChanged: function (msg) {
      this.statusMessage = msg;
    },
    openSession: function() {
      var self = this;
      self.contentConsole = "";
      fetch(encodeURI(((self.sessionOpened)? "/session/close" : "/session/open")), {
        method: "POST",
        headers: {
          "Content-Type": "application/json;charset=utf-8",
        },
        body: JSON.stringify({
          
        }),
        cache: "no-store",
      })
        .then(function (response) {
          return response.json();
        })
        .then(function (data) {
          self.updateSessionStatus();
        });
    },
    updateSessionStatus: function() {
      var self = this;
      fetch(encodeURI("/session/status"), {
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
          if(!self.sessionOpened && (data.data)) {
            if(self.socket) {
              self.socket.close();
            }
            
            self.socket = new WebSocket('ws://localhost:' + portWs.toString() + '/');
            self.socket.addEventListener('open',function(e){ });
            self.socket.addEventListener('message',function(e){
                self.contentConsole = self.contentConsole + e.data;
            });
          }
          
          if(!(data.data) && (self.socket != null)) {
            self.socket.close();
            self.socket = null;
          }
          
          self.sessionOpened = data.data;
        });
    }
    ,evaluate: function(multithread) {
      var self = this;
      this.contentConsole += this.editorConsoleInput.getSelectedText() + "\n";
      console.log(this.editorConsoleInput.getSelectedText())
      
      fetch(encodeURI("/editor/eval"), {
        method: "POST",
        body: JSON.stringify({
          src: self.editorConsoleInput.getSelectedText(),
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
    ,newFile: function() {
      if((this.selectedItem == null) 
        || (this.selectedItem.entry !== "directory")) {
        return;
      }
      
      this.showFileCreateModal = true;
    }
    ,openNewFile: function() {
      var tabName = "TAB:" + app.tabs.length.toString();
      app.tabs.push({
        name: tabName,
        src: `/editor?target=${this.selectedItem.path}${this.nameNewFile}`,
        dispName: this.nameNewFile, 
      });
      this.selectedTab = tabName;
      
      if(this.showFileCreateModal) {
        this.showFileCreateModal = false;
      }
    }
    ,cancelNewFile: function() {
      this.showFileCreateModal = false;
    }
    ,refreshDirectory: function() {
      if((this.selectedItem == null) 
        && (this.selectedItem.entry !== "directory")) {
        return;
      }
      this.selectedItem.children = [];
      this.refreshFilelist(this.selectedItem.path);
    }
    ,showFileDelete: function() {
      if((this.selectedItem == null) 
        || (this.selectedItem.entry !== "file")) {
        return;
      }
      
      this.showFileDeleteModal = true;
    }
    ,deleteFile: function() {
      var self = this;
      
      this.showFileDeleteModal = false;
      
      if((this.selectedItem == null) 
        || (this.selectedItem.entry !== "file")) {
        return;
      }
     
      fetch(encodeURI("/file/delete"), {
        method: "POST",
        body: JSON.stringify({
          path: self.selectedItem.path,
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
          //$("#modal2").modal("show");
          alert("File deleted!");
        }); 
    }
    ,cancelDeleteFile: function() {
      this.showFileDeleteModal = false;
    }
    ,onResizeSouth: function(south) {
      this.southSize = south;
      storageUtil.setStorage("southSize", south);
    }
    ,onResizeSouthEast: function(east) {
      this.southEastSize = east;
      storageUtil.setStorage("southEastSize", east);
    }
  },
});

/*
 * Register shortcut
 */
shortcut.add("Ctrl+Shift+B", function () {
  app.evaluate(true);
});

shortcut.add("Ctrl+B", function () {
  app.evaluate(false);
});

// Refresh file list
app.refreshFilelist("");

// Update session status
app.updateSessionStatus();

// Check session status
setInterval(function() {
  app.updateSessionStatus();
}, 3000);