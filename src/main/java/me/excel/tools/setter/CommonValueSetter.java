package me.excel.tools.setter;

import me.excel.tools.FieldUtils;
import me.excel.tools.model.excel.ExcelCell;

import java.util.function.BiConsumer;

/**
 * 自定义value setter
 * <p>
 * Created by hanwen on 16-1-7.
 */
public class CommonValueSetter<D> extends AbstractCellValueSetter {

  protected BiConsumer<D, ExcelCell> valueSetter;

  public CommonValueSetter(String matchField) {
    super(matchField);
  }

  public CommonValueSetter(String matchField, BiConsumer<D, ExcelCell> valueSetter) {
    super(matchField);
    this.valueSetter = valueSetter;
  }

  @Override
  public void set(Object data, ExcelCell excelCell) {
    boolean isNullSet = false;
    if (excelCell.getValue() == null) {
      isNullSet = setNullValue(data, excelCell.getField());
    }
    if (!isNullSet) {
      valueSetter.accept((D) data, excelCell);
    }
  }

  private boolean setNullValue(Object data, String fieldName) {

    Class fieldType = FieldUtils.getFieldType(data.getClass(), FieldUtils.getFieldWithoutPrefix(fieldName).split("\\."));

    if (fieldType != null) {
      // 已在reflectValueSetter中处理过
      return true;
    }

    return false;
  }
}
