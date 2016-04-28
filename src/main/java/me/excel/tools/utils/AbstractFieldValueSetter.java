package me.excel.tools.utils;


import me.excel.tools.model.excel.ExcelCell;

/**
 * model的field value setter父类, 提供默认的value setter,<br/>
 * 默认的value setter 支持{@link String}, {@link Integer}, {@link Double}, {@link Boolean},<br/>
 * 自定义value setter 用 {@link CommonValueSetter}
 *
 * Created by hanwen on 15-12-16.
 */
public abstract class AbstractFieldValueSetter implements FieldValueSetter {

  protected String matchField;

  public AbstractFieldValueSetter(String matchField) {
    this.matchField = matchField;
  }

  @Override
  public abstract void set(Object data, ExcelCell excelCell);

  @Override
  public boolean matches(ExcelCell excelCell) {
    return excelCell.getField().equals(matchField);
  }

  @Override
  public boolean matches(String field) {
    return field.equals(matchField);
  }
}
