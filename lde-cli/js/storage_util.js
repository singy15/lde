/*
 * Utility for localStorage
 */
class StorageUtil {
  /*
   * Ctor
   */
  constructor(rootKey) {
    this.rootKey = rootKey;
  }
  
  /*
   * Save to storage
   */
  setStorage(key, obj) {
    localStorage.setItem(this.rootKey + key, JSON.stringify(obj));
  }
  
  /*
   * Get from storage
   */
  getStorage(key, dflt) {
    var item = localStorage.getItem(this.rootKey + key);
    return item ? JSON.parse(item) : dflt;
  }
}

export { StorageUtil };