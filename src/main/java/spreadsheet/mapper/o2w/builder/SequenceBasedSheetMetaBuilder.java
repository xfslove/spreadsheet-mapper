package spreadsheet.mapper.o2w.builder;

import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.model.meta.SheetMetaBean;

import java.util.ArrayList;
import java.util.List;

/**
 * {@link SheetMeta} builder, build start at cell[1,1]
 * <p>
 * Created by hanwen on 2017/1/10.
 */
public class SequenceBasedSheetMetaBuilder {

  private int columnIndex = 1;
  int rowIndex = 1;
  List<FieldMeta> fieldMetas = new ArrayList<>();

  /**
   * create a field meta and add at {@link #columnIndex}, after add {@link #columnIndex} will plus 1
   *
   * @param prefix {@link FieldMeta#getPrefix()}
   * @param name   {@link FieldMeta#getName()}
   * @return this
   */
  public SequenceBasedFieldMetaBuilder field(String prefix, String name) {
    SequenceBasedFieldMetaBuilder sequenceBasedFieldMetaBuilder = new SequenceBasedFieldMetaBuilder(prefix, name, columnIndex);
    sequenceBasedFieldMetaBuilder.setSequenceBasedSheetMetaBuilder(this);
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
}
