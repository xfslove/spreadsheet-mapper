package me.excel.tools.model.ext;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by hanwen on 2016/12/28.
 */
public class SheetHeaderBean implements SheetHeader {

  private HeaderMeta headerMeta;

  // field to header text map
  private Map<String, String> values = new HashMap<>();

  public SheetHeaderBean(HeaderMeta headerMeta) {
    this.headerMeta = headerMeta;
  }

  @Override
  public void addValue(String field, String value) {
    this.values.put(field, value);
  }

  @Override
  public HeaderMeta getHeaderMeta() {
    return headerMeta;
  }

  @Override
  public String getValue(String field) {
    return values.get(field);
  }
}
