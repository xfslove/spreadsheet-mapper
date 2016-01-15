package me.excel.tools.utils;

import me.excel.tools.model.excel.ExcelCell;

import java.util.function.BiConsumer;

/**
 * 自定义value setter
 *
 * Created by hanwen on 16-1-7.
 */
public class CommonValueSetter<D> extends AbstractFieldValueSetter {

  protected BiConsumer<D, ExcelCell> associationValueSetter;

  public CommonValueSetter(String matchField) {
    super(matchField);
  }

  public CommonValueSetter(String matchField, BiConsumer<D, ExcelCell> associationValueSetter) {
    super(matchField);
    this.associationValueSetter = associationValueSetter;
  }

  @Override
  public void set(Object data, ExcelCell excelCell) {
    associationValueSetter.accept((D) data, excelCell);
  }
}
