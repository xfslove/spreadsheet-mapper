package me.excel.tools.utils;

import me.excel.tools.model.excel.ExcelCell;

import java.util.function.BiConsumer;

/**
 * 自定义value setter
 *
 * Created by hanwen on 16-1-7.
 */
public class CommonValueSetter<D> extends AbstractFieldValueSetter {

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
    valueSetter.accept((D) data, excelCell);
  }
}
