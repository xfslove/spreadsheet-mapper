package spreadsheet.mapper.o2w.compose.builder;

import spreadsheet.mapper.model.meta.*;

import java.util.ArrayList;
import java.util.List;

/**
 * {@link SheetMeta} builder, build start at cell[1,1]
 * <p>
 * Created by hanwen on 2017/1/10.
 */
public class SequenceBasedSheetMetaBuilder {

  private int columnIndex = 1;
  private int rowIndex = 1;
  private List<FieldMeta> fieldMetas = new ArrayList<>();

  /**
   * create a field meta and add at {@link #columnIndex}, after add {@link #columnIndex} will plus 1
   *
   * @param prefix {@link FieldMeta#getPrefix()}
   * @param name   {@link FieldMeta#getName()}
   * @return this
   */
  public SequenceBasedFieldMetaBuilder field(String prefix, String name) {
    SequenceBasedFieldMetaBuilder sequenceBasedFieldMetaBuilder = new SequenceBasedFieldMetaBuilder(prefix, name, columnIndex);
    columnIndex++;
    return sequenceBasedFieldMetaBuilder;
  }

  /**
   * @param name {@link FieldMeta#getName()}
   * @return this
   * @see #field(String, String)
   */
  public SequenceBasedFieldMetaBuilder field(String name) {
    return field(null, name);
  }

  /**
   * skip one column
   *
   * @return this
   */
  public SequenceBasedSheetMetaBuilder skip() {
    skip(1);
    return this;
  }

  /**
   * skip supplied numbers columns
   *
   * @param columnNum skip how much columns
   * @return this
   */
  public SequenceBasedSheetMetaBuilder skip(int columnNum) {
    columnIndex += columnNum;
    return this;
  }

  /**
   * data start at max header row index plus 1
   *
   * @return sheet meta
   * @see #toSheetMeta(int)
   */
  public SheetMeta toSheetMeta() {
    return toSheetMeta(rowIndex);
  }

  /**
   * to sheet meta
   *
   * @param dataStartRowIndex {@link SheetMeta#getDataStartRowIndex()}
   * @return {@link SheetMeta}
   */
  public SheetMeta toSheetMeta(int dataStartRowIndex) {
    return toSheetMeta(null, dataStartRowIndex);
  }

  public SheetMeta toSheetMeta(String sheetName, int dataStartRowIndex) {
    if (dataStartRowIndex < rowIndex) {
      throw new IllegalArgumentException("data start row index must be greater than max header row index[" + rowIndex + "]");
    }

    SheetMeta sheetMeta = new SheetMetaBean(sheetName, dataStartRowIndex);
    for (FieldMeta fieldMeta : fieldMetas) {
      sheetMeta.addFieldMeta(fieldMeta);
    }
    return sheetMeta;
  }

  /**
   * {@link FieldMeta} builder
   */
  public class SequenceBasedFieldMetaBuilder {

    private int rowIndex = 1;
    private FieldMeta fieldMeta;

    private SequenceBasedFieldMetaBuilder(String prefix, String name, int columnIndex) {
      this.fieldMeta = new FieldMetaBean(prefix, name, columnIndex);
    }

    /**
     * create a header meta and add at {@link #rowIndex}, after add {@link #rowIndex} will plus 1
     *
     * @param value {@link HeaderMeta#getValue()}
     * @return this
     */
    public SequenceBasedFieldMetaBuilder header(String value) {
      headers(value);
      return this;
    }

    /**
     * add header meta by sequence
     *
     * @param values {@link HeaderMeta#getValue()}
     * @return this
     * @see #header(String)
     */
    public SequenceBasedFieldMetaBuilder headers(String... values) {
      if (values == null) {
        return this;
      }
      for (String value : values) {
        fieldMeta.addHeaderMeta(new HeaderMetaBean(rowIndex, value));
        rowIndex++;
      }
      return this;
    }

    /**
     * skip one row
     *
     * @return this
     */
    public SequenceBasedFieldMetaBuilder skip() {
      skip(1);
      return this;
    }

    /**
     * skip supplied numbers rows
     *
     * @param rowNum skip how much columns
     * @return this
     */
    public SequenceBasedFieldMetaBuilder skip(int rowNum) {
      rowIndex += rowNum;
      return this;
    }

    /**
     * finish one field create, go to next
     *
     * @return {@link SequenceBasedSheetMetaBuilder}
     */
    public SequenceBasedSheetMetaBuilder next() {
      SequenceBasedSheetMetaBuilder.this.rowIndex = Math.max(this.rowIndex, SequenceBasedSheetMetaBuilder.this.rowIndex);
      SequenceBasedSheetMetaBuilder.this.fieldMetas.add(fieldMeta);
      return SequenceBasedSheetMetaBuilder.this;
    }

  }
}
