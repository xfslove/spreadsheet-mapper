package java.excel.engine.model.ext;

import java.excel.engine.model.excel.Sheet;

import java.util.List;

/**
 * the context corresponding excel sheet data at {@link #getSheetIndex()}.
 * <p>
 * Created by hanwen on 2016/12/29.
 */
public interface SheetContext {

  /**
   * <pre>
   * add sheet headers, the values using fields({@link #getFields()}) sequence to corresponding.
   * header meta unique with row index in one sheet (one to one),
   * if you add meta with same row index ({@link HeaderMeta#getRowIndex()}),
   * after add will override before add
   * </pre>
   *
   * @param meta   which header
   * @param values header text values
   */
  void addHeader(HeaderMeta meta, String... values);

  /**
   * <pre>
   * add sheet header.
   * header meta unique with row index in one sheet (one to one),
   * if you add meta with same row index ({@link HeaderMeta#getRowIndex()}),
   * after add will override before add
   * </pre>
   *
   * @param sheetHeader header
   */
  void addHeader(SheetHeader sheetHeader);

  /**
   * <pre>
   * set sheet headers.
   * header meta unique with row index in one sheet (one to one),
   * if you add meta with same row index ({@link HeaderMeta#getRowIndex()}),
   * after add will override before add
   * </pre>
   *
   * @param sheetHeaders headers
   */
  void setSheetHeaders(List<SheetHeader> sheetHeaders);

  /**
   * @return sheet index
   * @see Sheet#getIndex()
   */
  int getSheetIndex();

  /**
   * @return sheet name
   * @see Sheet#getName()
   */
  String getSheetName();

  /**
   * @return data start row index
   * @see SheetTemplate#getDataStartRowIndex()
   */
  int getDataStartRowIndex();

  /**
   * @return the object list corresponding one sheet data
   */
  List<Object> getData();

  /**
   * @return sheet template
   * @see SheetTemplate
   */
  SheetTemplate ofTemplate();

  /**
   * @return current context supplied which fields value of data ({@link #getData()})
   */
  List<String> getFields();

  /**
   * @return sheet header
   * @see SheetHeader
   */
  List<SheetHeader> getSheetHeaders();
}
