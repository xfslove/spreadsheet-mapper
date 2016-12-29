package me.excel.tools.model.ext;

import java.util.Objects;

/**
 * Created by hanwen on 2016/12/29.
 */
public class HeaderMetaBean implements HeaderMeta {

  private int rowIndex;

  private String name;

  public HeaderMetaBean(int rowIndex, String name) {
    this.rowIndex = rowIndex;
    this.name = name;
  }

  public static HeaderMeta DEFAULT_TITLE() {
    return new HeaderMetaBean(1, META_TITLE);
  }

  public static HeaderMeta DEFAULT_FIELD() {
    return new HeaderMetaBean(2, META_FIELD);
  }

  public static HeaderMeta DEFAULT_PROMPT() {
    return new HeaderMetaBean(3, META_PROMPT);
  }

  @Override
  public int getRowIndex() {
    return rowIndex;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    HeaderMetaBean that = (HeaderMetaBean) o;
    return rowIndex == that.rowIndex &&
        Objects.equals(name, that.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(rowIndex, name);
  }
}
