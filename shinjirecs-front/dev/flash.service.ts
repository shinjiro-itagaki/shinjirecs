import {Injectable} from '@angular/core'

export interface FlashServiceObserver {
  flashBeSet(key: string): void;
}

@Injectable()
export class FlashService {
  private _v: {[key: string]: string;} = {};
  public _observer: FlashServiceObserver = null; 
  
  get(key: string): string {
    var v: string = this._v[key];
    delete this._v[key]
    return v;
  }
  set(key: string, value: string): string {
    this._v[key] = value;
    if(this._observer != null) {
      this._observer.flashBeSet(key);
    }
    return value;
  }
  exists(key: string): boolean {
    return this._v[key] != null;
  }
}
