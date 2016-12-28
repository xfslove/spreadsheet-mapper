package me.excel.tools.setter;


import me.excel.tools.model.excel.Cell;

/**
 * field value setter adapter, easy implements customer value setter extends this.
 * <p>
 * Created by hanwen on 15-12-16.
 */
public abstract class FieldValueSetterAdapter implements FieldValueSetter {

  private String matchField;

  public FieldValueSetterAdapter(String matchField) {
    this.matchField = matchField;
  }

  @Override
  public String getMatchField() {
    return matchField;
  }

  @Override
  public abstract void set(Object data, Cell cell);
}
