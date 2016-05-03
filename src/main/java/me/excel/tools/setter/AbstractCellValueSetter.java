package me.excel.tools.setter;


import me.excel.tools.model.excel.ExcelCell;

/**
 * model的field value setter父类, 提供默认的value setter,<br/>
 * 默认的value setter 支持{@link String}, {@link Integer}, {@link Double}, {@link Boolean},<br/>
 * 自定义value setter 用 {@link CommonValueSetter}
 *
 * Created by hanwen on 15-12-16.
 */
public abstract class AbstractCellValueSetter implements CellValueSetter {

  protected String matchField;

  public AbstractCellValueSetter(String matchField) {
    this.matchField = matchField;
  }

  @Override
  public String getMatchField() {
    return matchField;
  }

  @Override
  public abstract void set(Object data, ExcelCell excelCell);

  @Override
  public boolean matches(ExcelCell excelCell) {
    return excelCell.getField().equals(matchField);
  }
}
