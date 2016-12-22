package me.excel.tools.setter;


import me.excel.tools.model.excel.ExcelCell;

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

  protected final String getMatchField() {
    return matchField;
  }

  @Override
  public abstract void set(Object data, ExcelCell excelCell);

  @Override
  public boolean matches(String field) {
    return matchField.equals(field);
  }
}
