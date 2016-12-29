package java.excel.engine.importer.setter;

import java.excel.engine.model.excel.Cell;

import java.util.function.BiConsumer;

/**
 * customer value setter
 * <p>
 * Created by hanwen on 16-1-7.
 */
public class CommonValueSetter<D> extends FieldValueSetterAdapter {

  private BiConsumer<D, Cell> valueSetter;

  public CommonValueSetter(String matchField) {
    super(matchField);
  }

  public CommonValueSetter(String matchField, BiConsumer<D, Cell> valueSetter) {
    super(matchField);
    this.valueSetter = valueSetter;
  }

  @Override
  public void set(Object data, Cell cell) {
    valueSetter.accept((D) data, cell);
  }

}
