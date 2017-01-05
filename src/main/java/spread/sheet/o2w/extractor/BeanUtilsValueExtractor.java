package spread.sheet.o2w.extractor;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spread.sheet.model.meta.FieldMeta;
import spread.sheet.o2w.composer.WorkbookComposeException;
import spread.sheet.utils.FieldUtils;


/**
 * using {@link BeanUtils#getProperty(Object, String)}, this use to fallback
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class BeanUtilsValueExtractor<T> implements ValueExtractor<T> {

  private static final Logger LOGGER = LoggerFactory.getLogger(BeanUtilsValueExtractor.class);

  @Override
  public String getStringValue(T object, FieldMeta fieldMeta) {
    try {
      return BeanUtils.getProperty(object, FieldUtils.detectRealField(fieldMeta.getName()));
    } catch (NestedNullException e) {
      LOGGER.trace(ExceptionUtils.getStackTrace(e));
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookComposeException(e);
    }
  }

}
