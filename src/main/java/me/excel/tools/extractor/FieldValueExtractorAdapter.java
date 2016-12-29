package me.excel.tools.extractor;

/**
 * field value extractor adapter, easy implements customer value extractor extends this.
 * <p>
 * Created by hanwen on 5/3/16.
 */
public abstract class FieldValueExtractorAdapter implements FieldValueExtractor {

  private int sheetIndex;

  private String matchField;

  public FieldValueExtractorAdapter(String matchField) {
    this.sheetIndex = 1;
    this.matchField = matchField;
  }

  public FieldValueExtractorAdapter(int sheetIndex, String matchField) {
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
  public abstract String getStringValue(Object data);
}
