package java.excel.engine.importer.setter;


import java.excel.engine.model.excel.Cell;

/**
 * field value setter adapter, easy implements customer value setter extends this.
 * <p>
 * Created by hanwen on 15-12-16.
 */
public abstract class FieldValueSetterAdapter implements FieldValueSetter {

  private int sheetIndex;

  private String matchField;

  public FieldValueSetterAdapter(String matchField) {
    this.sheetIndex = 1;
    this.matchField = matchField;
  }

  public FieldValueSetterAdapter(int sheetIndex, String matchField) {
    this.sheetIndex = sheetIndex;
    this.matchField = matchField;
  }

  @Override
  public String getMatchField() {
    return matchField;
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

  @Override
  public abstract void set(Object data, Cell cell);
}
